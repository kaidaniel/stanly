
#include <algorithm>
#include <boost/concept_check.hpp>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include "fields.h"
#include "string-index.h"
#include "symbols.h"
#include "syntax.h"
#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {

class cursor {
  TSTreeCursor cursor_{};
  std::string_view program_;
  std::function<void()> destroy;
  TSNode
  node() {
    return ts_tree_cursor_current_node(&cursor_);
  }
  std::string_view
  text(const TSNode node) {
    return {program_.begin() + ts_node_start_byte(node), program_.begin() + ts_node_end_byte(node)};
  };

 public:
  cursor(std::string_view program) : program_(program) {
    auto* parser = ts_parser_new();
    ts_parser_set_language(parser, tree_sitter_python());
    auto* tree = ts_parser_parse_string(parser, nullptr, program_.begin(), program_.size());
    cursor_ = ts_tree_cursor_new(ts_tree_root_node(tree));
    destroy = [parser, tree, cursor = &cursor_]() mutable {
      ts_parser_delete(parser);
      ts_tree_delete(tree);
      ts_tree_cursor_delete(cursor);
    };
  }
  cursor(const cursor&) = delete;
  cursor(cursor&&) = delete;
  cursor& operator=(const cursor&) = delete;
  cursor& operator=(const cursor&&) = delete;
  ~cursor() { destroy(); }
  TSSymbol
  symbol() {
    return ts_node_symbol(node());
  };
  void
  assert_at_symbol(symbols sym) {
    stanly_assert(symbol() == static_cast<TSSymbol>(sym));
  }
  TSFieldId
  field() {
    return ts_tree_cursor_current_field_id(&cursor_);
  };
  TSNode
  next_sibling() {
    return ts_node_next_named_sibling(ts_tree_cursor_current_node(&cursor_));
  }
  bool
  goto_child() {
    return ts_tree_cursor_goto_first_child(&cursor_);
  };
  bool
  goto_sibling() {
    return ts_tree_cursor_goto_next_sibling(&cursor_);
  };
  bool
  goto_parent() {
    return ts_tree_cursor_goto_parent(&cursor_);
  };
  void
  reset_cursor(TSNode node) {
    ts_tree_cursor_reset(&cursor_, node);
  }
  std::string_view
  text() {
    return text(node());
  }
  std::string_view
  text(fields field) {
    return text(ts_node_child_by_field_id(node(), static_cast<TSFieldId>(field)));
  }
  std::string
  parse_tree() {
    char* str = ts_node_string(node());
    std::string out{str};
    free(str);
    return out;
  }
};

using node_args = std::tuple<node, std::vector<std::string_view>>;
struct node_cursor : public cursor {
  using cursor::cursor;
  std::vector<node_args>
  parse_module(std::string_view tgt) {  // NOLINT
    assert_at_symbol(symbols::module);
    goto_child();
    std::vector<node_args> out{};
    auto simple_or_compound_statement = [&]() -> std::vector<node_args> {
      switch (static_cast<_simple_statement>(symbol())) {
        using enum _simple_statement;
        case assert_statement:
        case import_statement:
        case import_from_statement:
        case future_import_statement:
        case print_statement:
        case expression_statement:
        case return_statement:
        case delete_statement:
        case raise_statement:
        case pass_statement:
        case break_statement:
        case continue_statement:
        case nonlocal_statement:
        case global_statement:
        case exec_statement: unreachable("not implemented");
      }
      switch (static_cast<_compound_statement>(symbol())) {
        using enum _compound_statement;
        case if_statement:
        case match_statement:
        case for_statement:
        case while_statement:
        case try_statement:
        case with_statement:
        case function_definition:
        case decorated_definition:
        case class_definition: unreachable("not implemented yet.");
      }
      unreachable("module matched neither _simple_statement nor _compound_statement.");
    };
    do {  // module -> (_simple_statement | _compound_statement)*
      for (auto&& e : simple_or_compound_statement()) { out.push_back(std::move(e)); }
    } while (goto_sibling());
    return out;
  }
  std::vector<node_args>
  parse_dictionary(std::string_view tgt) {
    // dictionary("{" commaSep1(pair | dictionary_splat)? ","? "}")
    assert_at_symbol(symbols::dictionary);  // <dictionary(...)>
    std::vector<node_args> dict{{alloc{}, {tgt, "dict"}}};
    goto_child();                              // dictionary(<'{'> pair(...) ...)
    while (goto_sibling() && text() != "}") {  // dictionary(... <pair(...)> ...)
      // pair(key:expression ":" value:expression)
      assert_at_symbol(symbols::pair);
      dict.emplace_back(
          stanly::node{update{}}, std::vector{tgt, text(fields::key), text(fields::value)});
      goto_sibling();  // dictionary(... <','> ...)
    }
    goto_parent();  // <dictionary(...)>
    assert_at_symbol(symbols::dictionary);
    return dict;
  }
  std::tuple<std::string_view, std::string_view>
  parse_variable_and_field_from_subscript() {
    goto_child();
    assert_at_symbol(symbols::identifier);
    auto const variable = text();
    goto_sibling();  // skip '['
    goto_sibling();
    assert_at_symbol(symbols::identifier);
    return {variable, text()};
  };

  std::vector<node_args>
  parse_statement() {
    switch (static_cast<_simple_statement>(symbol())) {
      case _simple_statement::expression_statement: return parse_expression_statement();
      default: unreachable(std::format("parsing statement {} not yet implemented.", symbol()));
    }
  }

  std::vector<node_args>
  parse_expression(std::string_view var) {
    switch (static_cast<expression>(symbol())) {
      using enum expression;
      case as_pattern:
        goto_child();
        goto_sibling();
        return parse_expression(var);
      case await:
        goto_child();
        assert_at_symbol(symbols::expression);
        return parse_expression(var);
      case named_expression: {
        goto_child();
        assert_at_symbol(symbols::identifier);
        auto lhs = text();
        auto out = parse_expression(lhs);
        out.emplace_back(ref{}, std::vector{var, lhs});
        return out;
      }
      case not_operator: [[fallthrough]];
      case boolean_operator: [[fallthrough]];
      case comparison_operator: [[fallthrough]];
      case lambda: [[fallthrough]];
      case conditional_expression:
        return {{top{}, {var, std::format("expression {} not implemented", symbol())}}};
      case primary_expression: unreachable("only subtypes of primary_expression should appear.");
    }  // didn't return, so must be primary_expression.
    return parse_primary_expression(var);
  }
  std::vector<node_args>
  parse_primary_expression(std::string_view var) {
    std::string_view sv = text();
    switch (static_cast<primary_expression>(symbol())) {
      using enum primary_expression;
      case identifier: return {{ref{}, {var, sv}}};
      case string: return {{lit{}, {var, "str", sv}}};
      case integer: return {{lit{}, {var, "int", sv}}};
      case s_float: return {{lit{}, {var, "float", sv}}};
      case s_true: return {{lit{}, {var, "bool", sv}}};
      case s_false: return {{lit{}, {var, "bool", sv}}};
      case none: return {{lit{}, {var, "None", sv}}};
      case dictionary: return parse_dictionary(var);
      case set: [[fallthrough]];
      case list: return {{{alloc{}, {var, "top"}}}};
      case subscript: {
        auto [variable, field_] = parse_variable_and_field_from_subscript();
        return {{read{}, {var, variable, field_}}};
      }
      case ellipsis: return {{lit{}, {var, "Ellipsis", "ellipsis"}}};
      case binary_operator: [[fallthrough]];
      case unary_operator: [[fallthrough]];
      case attribute: [[fallthrough]];
      case call: [[fallthrough]];
      case tuple: [[fallthrough]];
      case list_comprehension: [[fallthrough]];
      case dictionary_comprehension: [[fallthrough]];
      case set_comprehension: [[fallthrough]];
      case generator_expression: [[fallthrough]];
      case parenthesized_expression: [[fallthrough]];
      case concatenated_string: unreachable(std::format("expression {} not implemented", symbol()));
    }
    unreachable("should have been either an expression or a primary_expression");
  }

  inline std::vector<node_args>
  parse_expression_statement() {
    assert_at_symbol(symbols::expression_statement);
    goto_child();
    switch (static_cast<expression_statement>(symbol())) {
      using enum expression_statement;
      case assignment: {
        goto_child();

        auto symbol_ = symbol();
        if (symbol_ == static_cast<TSSymbol>(primary_expression::identifier)) {
          auto const left = text();
          goto_sibling();  // assignment(left:identifier <"="> ...)
          goto_sibling();  // assignment(left:identifier "=" <right:...>)
          return parse_expression(left);
        }

        if (symbol_ == static_cast<TSSymbol>(primary_expression::subscript)) {
          auto [variable, field_] = parse_variable_and_field_from_subscript();
          goto_parent();
          goto_sibling();  // skip "="
          goto_sibling();
          assert_at_symbol(symbols::identifier);
          return {{update{}, {variable, field_, text()}}};
        };
      }
      case augmented_assignment:
        goto_child();
        assert_at_symbol(symbols::identifier);
        return {{top{}, {text(), "augmented assignment not implemented"}}};
      case expression: [[fallthrough]];
      case yield: unreachable("not implemented");
    }
    unreachable();
  }

  std::vector<node_args>
  parse_basic_block() {
    std::vector<node_args> node_args{};
    while (true) {
      auto sibling = next_sibling();
      std::ranges::move(parse_statement(), std::back_inserter(node_args));
      if (ts_node_is_null(sibling)) { break; }
      reset_cursor(sibling);
    }
    return node_args;
  }

  std::vector<node_args>
  parse_module() {
    goto_child();
    return parse_basic_block();
  }
};

std::vector<node>
parse(std::string&& program, string_index& idx) {
  auto cursor = node_cursor{idx.add_string_to_index(std::move(program))};
  std::vector<node> out{};
  for (auto&& [n, args] : cursor.parse_module()) { out.push_back(idx.set_handles(n, args)); }
  return out;
}
std::vector<node>
parse(std::string&& program) {
  return parse(std::move(program), global_string_index);
}

std::string
show_surface_syntax(std::string_view program) {
  return node_cursor{program}.parse_tree();
}

}  // namespace stanly
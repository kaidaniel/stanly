
#include <algorithm>
#include <boost/concept_check.hpp>
#include <ranges>
#include <stdexcept>
#include <string_view>
#include <vector>

#include "string-index.h"
#include "symbol-tables.h"
#include "syntax.h"
#include "tree_sitter/api.h"

namespace stanly {
using namespace syntax;

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
    cursor_ = ts_tree_cursor_new(ts_node_named_child(ts_tree_root_node(tree), 0));
    destroy = [parser, tree, cursor = &cursor_]() mutable {
      ts_parser_delete(parser);
      ts_tree_delete(tree);
      ts_tree_cursor_delete(cursor);
    };
    check_symbols();
  }
  cursor(const cursor&) = delete;
  cursor(cursor&&) = delete;
  cursor&
  operator=(const cursor&) = delete;
  cursor&
  operator=(const cursor&&) = delete;
  ~cursor() { destroy(); }
  TSSymbol
  symbol() {
    return ts_node_symbol(node());
  };
  template <class T>
  T
  assert_at_symbol(T sym) {
    stanly_assert(symbol() == static_cast<TSSymbol>(sym));
    return sym;
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
  text(TSFieldId field) {
    return text(ts_node_child_by_field_id(node(), field));
  }
};

using ast_node_args = std::tuple<ast_node, std::vector<std::string_view>>;
struct ast_node_cursor : public cursor {
  using cursor::cursor;
  using enum simple_statement;
  using enum compound_statement;
  std::vector<ast_node_args>
  parse_dictionary(std::string_view tgt) {
    // dictionary("{" commaSep1(pair | dictionary_splat)? ","? "}")
    assert_at_symbol(expression::dictionary);  // <dictionary(...)>
    std::vector<ast_node_args> dict{{alloc{}, {tgt, "dict"}}};
    goto_child();                              // dictionary(<'{'> pair(...) ...)
    while (goto_sibling() && text() != "}") {  // dictionary(... <pair(...)> ...)
      // pair(key:expression ":" value:expression)
      assert_at_symbol(dictionary::pair);
      dict.emplace_back(ast_node{update{}}, std::vector{tgt, text(fields.key), text(fields.value)});
      goto_sibling();  // dictionary(... <','> ...)
    }
    goto_parent();  // <dictionary(...)>
    assert_at_symbol(expression::dictionary);
    return dict;
  }
  std::tuple<std::string_view, std::string_view>
  parse_variable_and_field_from_subscript() {
    goto_child();
    stanly_assert(field() == fields.value);
    assert_at_symbol(expression::identifier);
    auto const variable = text();
    goto_sibling();  // skip '['
    goto_sibling();
    stanly_assert(field() == fields.subscript);
    assert_at_symbol(expression::identifier);
    return {variable, text()};
  };

  std::vector<ast_node_args>
  parse_statement() {
    switch (static_cast<simple_statement>(symbol())) {
      case expression_statement: return parse_expression_statement();
      default: unreachable(std::format("parsing statement {} not yet implemented.", symbol()));
    }
  }

  std::vector<ast_node_args>
  parse_expression(std::string_view var) {
    using enum expression;
    std::string_view sv = text();
    switch (static_cast<expression>(symbol())) {
      case as_pattern:
        goto_child();
        stanly_assert(field() == fields.alias);
        goto_sibling();
        stanly_assert(field() == fields.children);
        return parse_expression(var);
      case await:
        goto_child();
        assert_at_symbol(expression_statement::expression);
        return parse_expression(var);
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
        return {{load{}, {var, variable, field_}}};
      }
      case ellipsis: return {{lit{}, {var, "Ellipsis", "ellipsis"}}};
      case named_expression: {
        goto_child();
        assert_at_symbol(identifier);
        auto lhs = text();
        auto out = parse_expression(lhs);
        out.emplace_back(ref{}, std::vector{var, lhs});
        return out;
      }
      case not_operator: [[fallthrough]];
      case boolean_operator: [[fallthrough]];
      case binary_operator: [[fallthrough]];
      case unary_operator: [[fallthrough]];
      case comparison_operator: [[fallthrough]];
      case lambda: [[fallthrough]];
      case conditional_expression:
        return {{top{}, {var, std::format("expression {} not implemented", symbol())}}};
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
  }

  inline std::vector<ast_node_args>
  parse_expression_statement() {
    assert_at_symbol(expression_statement);
    goto_child();
    switch (static_cast<enum expression_statement>(symbol())) {
      case expression_statement::assignment: {
        goto_child();
        stanly_assert(field() == fields.left);

        auto symbol_ = symbol();
        if (symbol_ == static_cast<TSSymbol>(expression::identifier)) {
          auto const left = text();
          goto_sibling();  // assignment(left:identifier <"="> ...)
          goto_sibling();  // assignment(left:identifier "=" <right:...>)
          stanly_assert(field() == fields.right);
          return parse_expression(left);
        }

        if (symbol_ == static_cast<TSSymbol>(expression::subscript)) {
          auto [variable, field_] = parse_variable_and_field_from_subscript();
          goto_parent();
          goto_sibling();  // skip "="
          goto_sibling();
          assert_at_symbol(expression::identifier);
          return {{update{}, {variable, field_, text()}}};
        };
      }
      case expression_statement::augmented_assignment:
        goto_child();
        stanly_assert(field() == fields.left);
        assert_at_symbol(expression::identifier);
        return {{top{}, {text(), "augmented assignment not implemented"}}};
      case expression_statement::expression: [[fallthrough]];
      case expression_statement::yield: unreachable("not implemented");
    }
    unreachable();
  }

  std::vector<ast_node_args>
  parse_basic_block() {
    std::vector<ast_node_args> node_args{};
    while (true) {
      auto sibling = next_sibling();
      std::ranges::move(parse_statement(), std::back_inserter(node_args));
      if (ts_node_is_null(sibling)) { break; }
      reset_cursor(sibling);
    }
    return node_args;
  }
};

std::vector<ast_node>
parse(std::string&& program, string_index& idx) {
  auto cursor = ast_node_cursor{idx.add_string_to_index(std::move(program))};
  std::vector<ast_node> out{};
  for (auto&& [n, args] : cursor.parse_basic_block()) { out.push_back(idx.set_handles(n, args)); }
  return out;
}
std::vector<ast_node>
parse(std::string&& program) {
  return parse(std::move(program), global_string_index);
}

}  // namespace stanly
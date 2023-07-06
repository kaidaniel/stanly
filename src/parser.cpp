
#include <string_view>
#include <vector>

#include "string_index.h"
#include "syntax.h"
#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {
auto lookup_symbol(std::string_view) -> TSSymbol;
auto lookup_field(std::string_view) -> TSFieldId;
using namespace syntax;
struct symbols {
  TSSymbol expression_statement = lookup_symbol("expression_statement");
  TSSymbol assignment = lookup_symbol("assignment");
  TSSymbol module = lookup_symbol("module");
  TSSymbol identifier = lookup_symbol("identifier");
  TSSymbol integer = lookup_symbol("integer");
  TSSymbol string = lookup_symbol("string");
  TSSymbol dictionary = lookup_symbol("dictionary");
  TSSymbol pair = lookup_symbol("pair");
  TSSymbol list = lookup_symbol("list");
  TSSymbol set = lookup_symbol("set");
  TSSymbol subscript = lookup_symbol("subscript");
} const symbols{};
struct fields {
  TSFieldId left = lookup_field("left");
  TSFieldId right = lookup_field("right");
  TSFieldId key = lookup_field("key");
  TSFieldId value = lookup_field("value");
  TSFieldId subscript = lookup_field("subscript");
} const fields{};

class cursor {
  TSTreeCursor* cursor_;
  std::string_view program_;
  auto node() -> TSNode { return ts_tree_cursor_current_node(cursor_); }
  auto text(const TSNode node) -> std::string_view {
    return {program_.begin() + ts_node_start_byte(node), program_.begin() + ts_node_end_byte(node)};
  };

 public:
  cursor(TSTreeCursor* crsr, std::string_view program) : cursor_{crsr}, program_{program} {};
  auto symbol() -> TSSymbol { return ts_node_symbol(node()); };
  auto field() -> TSFieldId { return ts_tree_cursor_current_field_id(cursor_); };
  bool goto_child() { return ts_tree_cursor_goto_first_child(cursor_); };
  bool goto_sibling() { return ts_tree_cursor_goto_next_sibling(cursor_); };
  bool goto_parent() { return ts_tree_cursor_goto_parent(cursor_); };
  auto text() -> std::string_view { return text(node()); }
  auto text(TSFieldId field) -> std::string_view {
    return text(ts_node_child_by_field_id(node(), field));
  }
};

auto lookup_symbol(std::string_view name) -> TSSymbol {
  return ts_language_symbol_for_name(tree_sitter_python(), name.data(), name.length(), true);
}
auto lookup_field(std::string_view name) -> TSFieldId {
  return ts_language_field_id_for_name(tree_sitter_python(), name.data(), name.size());
}

using std::string_view;
using std::vector;
struct firstorder_cursor : public cursor {
  using cursor::cursor;
  auto parse_dictionary(std::string_view tgt, StringIndex& idx) -> vector<firstorder> {
    // dictionary("{" commaSep1(pair | dictionary_splat)? ","? "}")
    stanly_assert(symbol() == symbols.dictionary);  // <dictionary(...)>
    std::vector<firstorder> dictionary{alloc{idx.insert(tgt), idx.insert("dict")}};
    goto_child();                              // dictionary(<'{'> pair(...) ...)
    while (goto_sibling() && text() != "}") {  // dictionary(... <pair(...)> ...)
      // pair(key:expression ":" value:expression)
      stanly_assert(symbol() == symbols.pair);
      dictionary.emplace_back(
          update{idx.insert(tgt), idx.insert(text(fields.key)), idx.insert(text(fields.value))});
      goto_sibling();  // dictionary(... <','> ...)
    }
    goto_parent();  // <dictionary(...)>
    stanly_assert(symbol() == symbols.dictionary);
    return dictionary;
  }
  auto parse_variable_and_field_from_subscript() -> std::tuple<std::string_view, std::string_view> {
    goto_child();
    stanly_assert(field() == fields.value);
    stanly_assert(symbol() == symbols.identifier);
    auto const variable = text();
    goto_sibling();  // skip '['
    goto_sibling();
    stanly_assert(field() == fields.subscript);
    stanly_assert(symbol() == symbols.identifier);
    return {variable, text()};
  };

  auto parse_statement(StringIndex& idx) -> std::vector<firstorder> {
    stanly_assert(symbol() == symbols.expression_statement);
    goto_child();
    stanly_assert(symbol() == symbols.assignment);
    goto_child();
    stanly_assert(field() == fields.left);

    auto symbol_ = symbol();
    if (symbol_ == symbols.identifier) {
      auto const left = idx.insert(text());
      goto_sibling();  // assignment(left:identifier <"="> ...)
      goto_sibling();  // assignment(left:identifier "=" <right:...>)
      stanly_assert(field() == fields.right);

      symbol_ = symbol();
      auto const right = idx.insert(text());
      if (symbol_ == symbols.identifier) { return {ref{left, right}}; }
      if (symbol_ == symbols.string) { return {lit{left, idx.insert("str"), right}}; }
      if (symbol_ == symbols.integer) { return {lit{left, idx.insert("int"), right}}; }
      if (symbol_ == symbols.dictionary) { return parse_dictionary(idx.get_sv(left), idx); }
      if (symbol_ == symbols.set || symbol_ == symbols.list) {
        return {alloc{left, idx.insert("top")}};
      }
      if (symbol_ == symbols.subscript) {
        auto [variable, field_] = parse_variable_and_field_from_subscript();
        return {load{left, idx.insert(variable), idx.insert(field_)}};
      }
      unreachable();
    }

    if (symbol_ == symbols.subscript) {
      auto [variable, field_] = parse_variable_and_field_from_subscript();
      goto_parent();
      goto_sibling();  // skip "="
      goto_sibling();
      stanly_assert(symbol() == symbols.identifier);
      return {update{idx.insert(variable), idx.insert(field_), idx.insert(text())}};
    };

    unreachable();
  }
};
std::vector<firstorder> parse(std::string_view program, StringIndex idx = {}) {
  auto* parser = ts_parser_new();
  ts_parser_set_language(parser, tree_sitter_python());
  auto* tree = ts_parser_parse_string(parser, nullptr, program.begin(), program.size());
  auto cursor = ts_tree_cursor_new(ts_node_named_child(ts_tree_root_node(tree), 0));
  std::vector<firstorder> nodes{};
  while (true) {
    auto sibling = ts_node_next_named_sibling(ts_tree_cursor_current_node(&cursor));
    std::ranges::move(firstorder_cursor{&cursor, program}.parse_statement(idx),
                      std::back_inserter(nodes));
    if (ts_node_is_null(sibling)) { break; }
    ts_tree_cursor_reset(&cursor, sibling);
  }
  ts_parser_delete(parser);
  ts_tree_delete(tree);
  ts_tree_cursor_delete(&cursor);
  return nodes;
}

}  // namespace stanly
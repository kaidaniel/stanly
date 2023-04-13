#pragma once
#include <tree_sitter/api.h>

#include <cassert>
#include <exception>
#include <string>

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly::parser {
TSSymbol lookup_symbol(std::string_view name) {
  return ts_language_symbol_for_name(tree_sitter_python(), name.data(), name.length(), true);
}
TSFieldId lookup_field(std::string_view name) {
  return ts_language_field_id_for_name(tree_sitter_python(), name.data(), name.size());
}
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

template <typename S>
typename S::node parse_statement(TSTreeCursor*, std::string_view);
}  // namespace stanly::parser
namespace stanly {
template <typename S>
std::vector<typename S::node> parse(std::string_view program) {
  auto* parser = ts_parser_new();
  if (!ts_parser_set_language(parser, tree_sitter_python())) {
    throw std::domain_error{"couldn't set treesitter language"};
  };
  auto* tree = ts_parser_parse_string(parser, nullptr, program.begin(), program.size());
  assert(ts_node_symbol(ts_tree_root_node(tree)) == parser::symbols.module);
  auto cursor = ts_tree_cursor_new(ts_node_named_child(ts_tree_root_node(tree), 0));

  std::vector<typename S::node> statements{};
  while (true) {
    auto next_named_sibling = ts_node_next_named_sibling(ts_tree_cursor_current_node(&cursor));
    statements.push_back(parser::parse_statement<S>(&cursor, program));
    if (ts_node_is_null(next_named_sibling)) { break; }
    ts_tree_cursor_reset(&cursor, next_named_sibling);
  }

  ts_tree_delete(tree);
  ts_parser_delete(parser);
  ts_tree_cursor_delete(&cursor);
  return statements;
}
}  // namespace stanly
#pragma once
#include <tree_sitter/api.h>

#include <exception>
#include <string>

#include "any"
#include "stanly-utils.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {
auto lookup_symbol(std::string_view) -> TSSymbol;
auto lookup_field(std::string_view) -> TSFieldId;
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

class parser {
  template <class T, auto Deleter>
  using uptr = std::unique_ptr<T, decltype([](T* t) { Deleter(t); })>;
  uptr<TSParser, ts_parser_delete> parser_;
  uptr<TSTree, ts_tree_delete> tree_;
  TSTreeCursor cursor_;

 public:
  parser(std::string_view program);
  ~parser();
  parser(const parser&) = delete;
  parser& operator=(const parser&) = delete;
  parser(parser&&) = delete;
  parser& operator=(parser&&) = delete;
  TSTreeCursor* cursor();
};
class cursor {
  TSTreeCursor* cursor_;
  std::string_view program_;
  auto node() -> TSNode;
  auto text(TSNode) -> std::string_view;

 public:
  cursor(TSTreeCursor*, std::string_view);
  auto field() -> TSFieldId;
  auto symbol() -> TSSymbol;
  auto goto_child() -> bool;
  auto goto_sibling() -> bool;
  auto goto_parent() -> bool;
  auto text() -> std::string_view;
  auto text(TSFieldId) -> std::string_view;
};

}  // namespace stanly
namespace stanly {
template <class Result>
Result parse_statement(TSTreeCursor*, std::string_view);
template <class Node>
std::vector<Node> parse(std::string_view program) {
  parser prsr{program};
  std::vector<Node> nodes{};
  while (true) {
    auto sibling = ts_node_next_named_sibling(ts_tree_cursor_current_node(prsr.cursor()));
    nodes.push_back(parse_statement<Node>(prsr.cursor(), program));
    if (ts_node_is_null(sibling)) { break; }
    ts_tree_cursor_reset(prsr.cursor(), sibling);
  }
  return nodes;
}

}  // namespace stanly
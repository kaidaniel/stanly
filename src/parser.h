#pragma once

#include <tree_sitter/api.h>

#include <concepts>
#include <cstring>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace stanly {
class parser {
  std::string_view program_;
  TSLanguage *language_;
  TSParser *parser_;
  TSTree *tree_;
  TSNode root_;
  TSTreeCursor cursor_;
  struct Symbols {
    TSSymbol expression_statement;
    TSSymbol assignment;
    TSSymbol module;
    TSSymbol identifier;
    TSSymbol integer;
    TSSymbol string;
    TSSymbol dictionary;
    TSSymbol pair;
    TSSymbol list;
    TSSymbol set;
    TSSymbol subscript;
  } symbols_;
  struct Fields {
    TSFieldId left;
    TSFieldId right;
    TSFieldId key;
    TSFieldId value;
    TSFieldId subscript;
  } fields_;

  [[nodiscard]] const TSNode &root() const;
  [[nodiscard]] TSSymbol symbol(const std::string &name) const;
  [[nodiscard]] TSFieldId field(const std::string &name) const;
  bool skip_concrete_nodes();
  bool to_child();
  bool to_parent();
  [[nodiscard]] TSSymbol symbol() const;
  bool at(const TSSymbol Symbols::*symbol);
  bool at(const TSFieldId Fields::*field);
  bool at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol);
  [[nodiscard]] std::string type() const;
  [[nodiscard]] std::unique_ptr<char> s_expr() const;
  std::string_view text();
  std::vector<std::string_view> record();
  [[nodiscard]] TSNode node() const;

 public:
  ~parser();
  parser(const parser &) = delete;
  parser operator=(const parser &) = delete;
  parser(parser &&) = delete;
  parser operator=(parser &&) = delete;

  explicit parser(std::string_view program);
  template <typename S>
  [[nodiscard]] typename S::node parse_statement();
  bool to_sibling();
  [[nodiscard]] TSTreeCursor copy_cursor() const;
  void set_cursor(TSTreeCursor);
};

template <typename S>
std::vector<typename S::node> parse(std::string_view program) {
  std::vector<typename S::node> ast{};
  parser parser{program};
  do {
    auto cursor = parser.copy_cursor();
    ast.push_back(parser.parse_statement<S>());
    parser.set_cursor(cursor);
  } while (parser.to_sibling());
  return ast;
}
}  // namespace stanly

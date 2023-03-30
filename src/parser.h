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
  } symbols_;
  struct Fields {
    TSFieldId left;
    TSFieldId right;
    TSFieldId key;
    TSFieldId value;
  } fields_;

  [[nodiscard]] const TSNode &root() const;
  [[nodiscard]] TSSymbol symbol(const std::string &name) const;
  [[nodiscard]] TSFieldId field(const std::string &name) const;
  bool skip_concrete_nodes();
  bool to_child();
  bool to_sibling();
  bool to_parent();
  [[nodiscard]] TSNode node() const;
  [[nodiscard]] TSSymbol symbol() const;
  bool at(const TSSymbol Symbols::*symbol);
  bool at(const TSFieldId Fields::*field);
  bool at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol);
  [[nodiscard]] std::string type() const;
  std::string_view text();
  std::vector<std::string_view> record();

 public:
  explicit parser(std::string_view program);
  ~parser();
  parser(const parser &) = delete;
  parser operator=(const parser &) = delete;
  parser(parser &&) = delete;
  parser operator=(parser &&) = delete;

  template <typename S>
  typename S::node next_node();

  [[nodiscard]] bool is_done() const;
};

template <typename S>
std::vector<typename S::node> parse_language(std::string_view program) {
  std::vector<typename S::node> ast{};
  parser parser{program};
  while (!parser.is_done()) {
    ast.push_back(parser.next_node<S>());
  }
  return ast;
}
}  // namespace stanly

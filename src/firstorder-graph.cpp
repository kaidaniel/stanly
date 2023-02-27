#include "firstorder-lang.h"
#include "stanly-api.h"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <iostream>
#include <numeric>
#include <string>
#include <tree_sitter/api.h>
#include <tree_sitter/parser.h>
#include <vector>

extern "C" {
TSLanguage *tree_sitter_python(void);
}
namespace stanly {

template <class... Args> void FirstOrderGraph::insert(Args &&...args) {
  nodes_.emplace_back(args...);
};

std::string show(const DeclareLocalVar &) { return "(DeclareLocalVar ?)"; }
std::string show(const SetField &) { return "(SetField ? ? ?)"; }
std::string show(const LoadField &n) {
  return std::string("(LoadField ") + std::to_string(n.lhs) + " " +
      std::to_string(n.source) + " " + std::to_string(n.field) + ")";
}
std::string show(const LoadText &n) {
  return std::string("(LoadText ") + n.text_literal + " " +
      std::to_string(n.lhs) + ")";
}
std::string show(const LoadRecord &) { return "(LoadRecord ? ?)"; }
std::string show(const LoadVar &) { return "(LoadVar ? ?)"; }
std::string show(const FirstOrderGraph &graph) {
  return std::transform_reduce(
      std::begin(graph.nodes_), std::end(graph.nodes_), std::string{},
      [](const std::string &str1, const std::string &str2) {
        return str1 + str2;
      },
      [](const auto &syntax) {
        return std::visit([](const auto &node) { return show(node); }, syntax);
      });
}

namespace treesitter { // every use of tree-sitter in this namespace

  class Parser {
    TSLanguage *language_;
    TSParser *parser_;
    TSTree *tree_;
    TSNode root_;
  public:
    explicit Parser(const std::string &program)
        : language_(tree_sitter_python()),
          parser_(ts_parser_new()),
          tree_([&] {
            ts_parser_set_language(parser_, language_);
            return ts_parser_parse_string(
                parser_, nullptr, program.c_str(), program.length());
          }()),
          root_(ts_tree_root_node(tree_)) {}
    ~Parser() {
      ts_tree_delete(tree_);
      ts_parser_delete(parser_);
    }
    Parser(const Parser &) = delete;
    Parser operator=(const Parser &) = delete;
    Parser(Parser &&) = delete;
    Parser operator=(Parser &&) = delete;

    [[nodiscard]] const TSNode &root() const { return root_; }
    [[nodiscard]] static std::unique_ptr<char> s_expr(const TSNode& node) {
       return std::unique_ptr<char>{ts_node_string(node)};}
    [[nodiscard]] TSFieldId field_id(const std::string& name) const { 
      return ts_language_field_id_for_name(language_, name.c_str(), name.length());}
  };
  /* interface for Parser:
      root:   ()  -> Node
      child: Node -> Node
      name:  Node -> String */
  static FirstOrderGraph parse_firstorder(const std::string &program) {
    const Parser parser{program};
    TSNode child = ts_node_named_child(parser.root(), 0);
    std::string name = ts_node_type(child);
    FirstOrderGraph graph{};
    graph.insert(LoadText{0, name});
    graph.insert(LoadField{0, 1, 2});
    // std::cout << Parser::s_expr(child);
    // const TSFieldId STATEMENT = parser.field_id("expression_statement");
    std::cout << Parser::s_expr(parser.root());
    // TSTreeCursor cursor{ts_tree_cursor_new(parser.root())};
    // if(ts_tree_cursor_goto_first_child(&cursor)){
    //   while(ts_tree_cursor_goto_next_sibling(&cursor)){
    //     TSFieldId field = ts_tree_cursor_current_field_id(&cursor);
    //     if(field == STATEMENT) {

    //     }
    //   }
    // }


    return graph;
  }
} // namespace treesitter

Graph parse_firstorder(const std::string &program) {
  return Graph{treesitter::parse_firstorder, program};
}

} // namespace stanly

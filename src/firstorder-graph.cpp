#include "firstorder-lang.h"
#include "stanly-api.h"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <string>
#include <tree_sitter/api.h>
#include <vector>
TSLanguage *tree_sitter_python(void);
namespace stanly {

template <class... Args> void FirstOrderGraph::insert(Args &&...args) {
  nodes_.emplace_back(args...);
};

std::string show(const DeclareLocalVar &) { return "(DeclareLocalVar ?)"; }
std::string show(const SetField &) { return "(SetField ? ? ?)"; }
std::string show(const LoadField &) { return "(LoadField ? ? ?)"; }
std::string show(const LoadText &n) {
  return std::string("(LoadText ") + n.text_literal + " " +
      std::to_string(n.lhs) + ")";
}
std::string show(const LoadRecord &) { return "(LoadRecord ? ?)"; }
std::string show(const LoadVar &) { return "(LoadVar ? ?)"; }
std::string show(const FirstOrderSyntax &syntax) {
  return std::visit(
      [](const auto &node) { return show(node); }, syntax.variant_);
}
std::string show(const FirstOrderGraph &graph) { return show(graph.nodes_[0]); }

namespace treesitter { // every use of tree-sitter in this namespace

  class Parser {
    using Node = TSNode;
    TSParser *parser_;
    TSTree *tree_;
    Node root_;
  public:
    explicit Parser(const std::string &program)
        : parser_(ts_parser_new()),
          tree_(ts_parser_parse_string(
              parser_, nullptr, program.c_str(), program.length())),
          root_([&] {
            ts_parser_set_language(parser_, tree_sitter_python());
            return ts_tree_root_node(tree_);
          }()) {}
    ~Parser() {
      ts_tree_delete(tree_);
      ts_parser_delete(parser_);
    }
    Parser(const Parser &) = delete;
    Parser operator=(const Parser &) = delete;
    Parser(Parser &&) = delete;
    Parser operator=(Parser &&) = delete;

    [[nodiscard]] const Node &root() const { return root_; }
    [[nodiscard]] static Node child(const Node &node, uint32_t i) {
      return ts_node_named_child(node, i);
    }
    [[nodiscard]] static std::string name(const Node &node) {
      return ts_node_type(node);
    }
  };

  template <class Node> class ParserInterface {
    Node &root();
    Node child(Node &);
    std::string name(Node &);
  }

  static FirstOrderGraph
  parse_firstorder(const std::string &program) {
    const Parser parser{program};
    TSNode child = Parser::child(parser.root(), 0);

    return FirstOrderGraph{
        std::vector{FirstOrderSyntax{LoadText{0, Parser::name(child)}}}};
  }
} // namespace treesitter

Graph parse_firstorder(const std::string &program) {
  return Graph{treesitter::parse_firstorder(program)};
}
} // namespace stanly

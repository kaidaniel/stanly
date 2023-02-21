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
    TSParser *parser_;
    TSTree *tree_;
  public:
    Parser(const std::string &program)
        : parser_(ts_parser_new()),
          tree_(ts_parser_parse_string(
              parser_, nullptr, program.c_str(), program.length())) {
      ts_parser_set_language(parser_, tree_sitter_python());
    }
    ~Parser() {
      ts_tree_delete(tree_);
      ts_parser_delete(parser_);
    }
    Parser(const Parser &) = delete;
    Parser operator=(const Parser &) = delete;
    Parser(Parser &&) = delete;
    Parser operator=(Parser &&) = delete;

    [[nodiscard]] TSNode root() const { return ts_tree_root_node(tree_); }
    [[nodiscard]] static TSNode child(TSNode node, uint32_t i) {
      return ts_node_named_child(node, i);
    }
  };

  static std::vector<FirstOrderSyntax>
  parse_firstorder(const std::string &program) {
    const Parser parser{program};
    TSNode root = parser.root();
    TSNode child = Parser::child(root, 0);

    return std::vector{FirstOrderSyntax{LoadText{0, ts_node_type(child)}}};
  }
} // namespace treesitter

Graph parse_firstorder(const std::string &program) {
  return Graph{FirstOrderGraph{treesitter::parse_firstorder(program)}};
}
} // namespace stanly

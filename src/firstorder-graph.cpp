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

using std::string;
using std::to_string;
using std::unique_ptr;
using str = const std::string &;
using std::begin;
using std::cout;
using std::end;
using std::transform_reduce;
using std::visit;

extern "C" {
TSLanguage *tree_sitter_python(void);
}
namespace stanly {

template <class... Args> void FirstOrderGraph::insert(Args &&...args) {
  nodes_.emplace_back(std::forward<Args>(args)...);
};

string show(const DeclareLocalVar &) { return "(DeclareLocalVar ?)"; }
string show(const SetField &) { return "(SetField ? ? ?)"; }
string show(const LoadField &n) {
  return string("(LoadField ") + to_string(n.lhs) + " " + to_string(n.source) +
      " " + to_string(n.field) + ")";
}
string show(const LoadText &n) {
  return string("(LoadText ") + n.text_literal + " " + to_string(n.lhs) + ")";
}
string show(const LoadRecord &) { return "(LoadRecord ? ?)"; }
string show(const LoadVar &) { return "(LoadVar ? ?)"; }
string show(const FirstOrderGraph &graph) {
  return transform_reduce(
      begin(graph.nodes_), end(graph.nodes_), string{},
      [](str str1, str str2) { return str1 + str2; },
      [](const auto &syntax) {
        return visit([](const auto &node) { return show(node); }, syntax);
      });
}

namespace treesitter { // every use of tree-sitter in this namespace

  class Parser {
    TSLanguage *language_;
    TSParser *parser_;
    TSTree *tree_;
    TSNode root_;
    TSTreeCursor cursor_;
    struct {
      TSFieldId statement;
      } fields_;
    FirstOrderGraph graph_{};
  public:
    explicit Parser(str program)
        : language_(tree_sitter_python()),
          parser_(ts_parser_new()),
          tree_([&] {
            ts_parser_set_language(parser_, language_);
            return ts_parser_parse_string(
                parser_, nullptr, program.c_str(), program.length());
          }()),
          root_(ts_tree_root_node(tree_)),
          cursor_(ts_tree_cursor_new(root_)),
          fields_({
            .statement=field_id("expression_statement")
            
          })
          {}
    ~Parser() {
      ts_tree_delete(tree_);
      ts_parser_delete(parser_);
    }
    Parser(const Parser &) = delete;
    Parser operator=(const Parser &) = delete;
    Parser(Parser &&) = delete;
    Parser operator=(Parser &&) = delete;

    [[nodiscard]] const TSNode &root() const { return root_; }
    [[nodiscard]] static unique_ptr<char> s_expr(const TSNode &node) {
      return unique_ptr<char>{ts_node_string(node)};
    }
    [[nodiscard]] TSFieldId field_id(str name) const {
      return ts_language_field_id_for_name(
          language_, name.c_str(), name.length());
    }
    template <class... Args> void insert(Args&& ...args) { graph_.insert(std::forward<Args>(args)...); }
    bool child() { return ts_tree_cursor_goto_first_child(&cursor_); }
    bool sibling() { return ts_tree_cursor_goto_next_sibling(&cursor_); }
    bool statement() { return ts_tree_cursor_current_field_id(&cursor_) == fields_.statement; }

    [[nodiscard]] FirstOrderGraph operator()() && {
      cout << s_expr(root());
      if(child()){
        while(sibling()) {
          if(statement()){
            insert(LoadText{0, "x"});
          }
        }
      }
      return graph_;
    }
  };

  static FirstOrderGraph parse_firstorder(str program) {
    return Parser{program}();
  }
} // namespace treesitter

Graph parse_firstorder(const std::string &program) {
  return Graph{treesitter::parse_firstorder, program};
}

} // namespace stanly

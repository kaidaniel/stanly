#include "firstorder-lang.h"
#include "stanly-api.h"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <iostream>
#include <numeric>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tree_sitter/api.h>
//#include <tree_sitter/parser.h>
#include <vector>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <functional>

using std::string;
using std::to_string;
using std::unique_ptr;
using str = const std::string &;
using std::begin;
using std::cout;
using std::end;
using std::transform_reduce;
using std::visit;
using std::string_view;
using fmt::format;
using std::function;



extern "C" {
TSLanguage *tree_sitter_python(void);
}
namespace stanly {
using GetVariable = const function<string_view(Var)>&;
FirstOrderGraph::FirstOrderGraph(std::string program) : program_(std::move(program)) {}

Var FirstOrderGraph::VariablePool::idx(string_view variable) {
    auto search = var_to_idx_.find(variable);
    if(search != end(var_to_idx_)) {return search->second;}
    max_++;
    var_to_idx_.emplace(variable, max_);
    idx_to_var_.push_back(variable);
    return max_;
  }
Var FirstOrderGraph::idx(string_view variable) { return variable_pool_.idx(variable); }
string_view FirstOrderGraph::VariablePool::var(Var idx) const {
  return idx_to_var_.at(idx);
}
string_view FirstOrderGraph::var(Var idx) const { return variable_pool_.var(idx); }


template <class... Args> void FirstOrderGraph::insert(Args &&...args) {
  nodes_.emplace_back(std::forward<Args>(args)...);
};

string show(const DeclareLocalVar &n, GetVariable get ) { 
  return format("(DeclareLocalVar {})", get(n.var)); 
}
string show(const SetField &n, GetVariable get) { 
  return format("(SetField {}[{}]={})", get(n.target), get(n.field), get(n.rhs)); 
  }
string show(const LoadField &n, GetVariable get) { 
  return format("(LoadField {}={}[{}])", get(n.lhs), get(n.source), get(n.field)); 
  }
string show(const LoadText &n, GetVariable get) {
  return format("(LoadText {}='{}')", get(n.lhs), n.text_literal); 
  }
string show(const LoadRecord &n, GetVariable get) { 
  return format("(LoadRecord {}={})", get(n.lhs), n.record_literal); 
  }
string show(const LoadVar &n, GetVariable get) { 
  return format("(LoadVar {}={})", get(n.lhs), get(n.rhs)); 
  }

string show(const FirstOrderGraph &graph) {
  return transform_reduce(
      begin(graph.nodes_), end(graph.nodes_), string{},
      [](str str1, str str2) { return str1 + str2; },
      [&graph](const auto &syntax) {
        return visit([&graph](const auto &node) { 
          return show(node, [&graph](Var v){return graph.var(v);}); }, syntax);
      });
}

namespace treesitter { // every use of tree-sitter in this namespace

  class Parser {
    FirstOrderGraph graph_;
    string_view program_;
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
      } symbols_;
    struct Fields {
      TSFieldId left;
      TSFieldId right;
    } fields_;
  public:
    explicit Parser(string_view program)
        : graph_(string{program}),
          program_(graph_.program()),
          language_(tree_sitter_python()),
          parser_(ts_parser_new()),
          tree_([&] {
            ts_parser_set_language(parser_, language_);
            return ts_parser_parse_string(
                parser_, nullptr, begin(program_), program_.size());
          }()),
          root_(ts_tree_root_node(tree_)),
          cursor_(ts_tree_cursor_new(root_)),
          symbols_({
            symbol("expression_statement"),
            symbol("assignment"),
            symbol("module"),
            symbol("identifier"),
            symbol("integer"),
            symbol("string"),
          }),
          fields_({
            field("left"),
            field("right"),
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
    [[nodiscard]] TSSymbol symbol(str name) const {
      return ts_language_symbol_for_name(
          language_, name.c_str(), name.length(), true);
    }
    [[nodiscard]] TSFieldId field(str name) const {
      return ts_language_field_id_for_name(language_, name.c_str(), name.size());
    }
    template <class... Args> void insert(Args&& ...args) { graph_.insert(std::forward<Args>(args)...); }
    void skip_concrete_nodes() { 
      while(not ts_node_is_named(ts_tree_cursor_current_node(&cursor_)) 
            and (to_sibling() or to_child())){ /* side-effects in loop head */}; }
    
    bool to_child() { 
      if(ts_tree_cursor_goto_first_child(&cursor_)){
        skip_concrete_nodes();
        return true;
      }
      return false;
    }
    void to_child(const TSSymbol Symbols::*symbol) { assert(to_child()); assert(at(symbol)); }
    void to_child(const TSFieldId Fields::*field) { assert(to_child()); assert(at(field)); }
    void to_child(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) {
      assert(to_child());
      assert(at(field));
      assert(at(symbol));
      
    }
    bool to_sibling() { 
      if(ts_tree_cursor_goto_next_sibling(&cursor_)){
        skip_concrete_nodes();
        return true;
      }
      return false;
    }
    void to_sibling(const TSSymbol Symbols::*symbol) { assert(to_sibling()); assert(at(symbol)); }
    void to_sibling(const TSFieldId Fields::*field) { assert(to_sibling()); assert(at(field)); }
    void to_sibling(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) { 
      assert(to_sibling()); 
      assert(at(field));
      assert(at(symbol));
    }

    TSSymbol symbol() { return ts_node_symbol(ts_tree_cursor_current_node(&cursor_)); }
    bool at(const TSSymbol Symbols::*symbol) { return ts_node_symbol(ts_tree_cursor_current_node(&cursor_)) == symbols_.*symbol; }
    bool at(const TSFieldId Fields::*field) { return ts_tree_cursor_current_field_id(&cursor_) == fields_.*field; }
    unique_ptr<char> s_expr() { return unique_ptr<char>{ts_node_string(ts_tree_cursor_current_node(&cursor_))}; }
    void show_type() { cout << ts_node_type(ts_tree_cursor_current_node(&cursor_)) << "\n"; }
    void show_field() { cout << ts_tree_cursor_current_field_name(&cursor_) << "\n"; }
    void show() { cout << s_expr() << "\n"; }
    string_view text() { 
      auto node = ts_tree_cursor_current_node(&cursor_);
      return {std::begin(program_) + ts_node_start_byte(node), std::begin(program_) + ts_node_end_byte(node)};
    }

    [[nodiscard]] FirstOrderGraph parse() && {
      show();
      assert(at(&Symbols::module));
      to_child(&Symbols::expression_statement);
      to_child(&Symbols::assignment);
      to_child(&Fields::left, &Symbols::identifier);
      Var lhs {graph_.idx(text())};
      to_sibling(&Fields::right);
      string_view rhs {text()};

      if (at(&Symbols::identifier)) {
        insert(LoadVar{.lhs=lhs, .rhs=graph_.idx(rhs)});
      } else if (at(&Symbols::string) or at(&Symbols::integer)) {
        insert(LoadText{.lhs=lhs, .text_literal=rhs});  
      } else {
        std::domain_error(format("assigning {} not implemented", rhs));
      }

      return graph_;
    }
  };

  static FirstOrderGraph parse_firstorder(string_view program) {
    return Parser{program}.parse();
  }
} // namespace treesitter

Graph parse_firstorder(string_view program) {
  return Graph{treesitter::parse_firstorder, program};
}

} // namespace stanly

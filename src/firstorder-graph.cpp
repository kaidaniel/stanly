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
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <functional>
#include <vector>

using std::string;
using std::unique_ptr;
using str = const std::string &;
using fmt::format;
using std::begin;
using std::cout;
using std::end;
using std::function;
using std::string_view;
using std::transform_reduce;
using std::visit;

extern "C" {
TSLanguage *tree_sitter_python(void);
}
namespace stanly {
using GetVariable = const function<string_view(VarIdx)> &;

VarIdx FirstOrderGraph::VariablePool::var_name_to_idx(string_view variable) {
  auto search = var_name_to_idx_.find(variable);
  if (search != end(var_name_to_idx_)) { return search->second; }
  max_++;
  var_name_to_idx_.emplace(variable, max_);
  var_idx_to_var_.push_back(variable);
  return max_;
}
VarIdx FirstOrderGraph::var_name_to_idx(string_view variable) {
  return variable_pool_.var_name_to_idx(variable);
}
string_view FirstOrderGraph::VariablePool::var_idx_to_name(VarIdx idx) const {
  return var_idx_to_var_.at(idx);
}
string_view FirstOrderGraph::var_idx_to_name(VarIdx idx) const {
  return variable_pool_.var_idx_to_name(idx);
}

template <class... Args> void FirstOrderGraph::insert(Args &&...args) {
  nodes_.emplace_back(std::forward<Args>(args)...);
};

string show(const DeclareLocalVar &n, GetVariable get) {
  return format("(DeclareLocalVar {})", get(n.var));
}
string show(const SetField &n, GetVariable get) {
  return format(
      "(SetField {}[{}]={})", get(n.target), get(n.field), get(n.rhs));
}
string show(const LoadField &n, GetVariable get) {
  return format(
      "(LoadField {}={}[{}])", get(n.lhs), get(n.source), get(n.field));
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
        return visit(
            [&graph](const auto &node) {
              return show(node, [&graph](VarIdx v) {
                return graph.var_idx_to_name(v);
              });
            },
            syntax);
      });
}

namespace treesitter { // every use of tree-sitter in this namespace

  class Parser {
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
      TSSymbol dictionary;
      TSSymbol pair;
    } symbols_;
    struct Fields {
      TSFieldId left;
      TSFieldId right;
      TSFieldId key;
      TSFieldId value;
    } fields_;
  public:
    explicit Parser(string_view program)
        : program_(program),
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
              .expression_statement = symbol("expression_statement"),
              .assignment = symbol("assignment"),
              .module = symbol("module"),
              .identifier = symbol("identifier"),
              .integer = symbol("integer"),
              .string = symbol("string"),
              .dictionary = symbol("dictionary"),
              .pair = symbol("pair"),
          }),
          fields_({
              .left = field("left"),
              .right = field("right"),
              .key = field("key"),
              .value = field("value"),
          }) {}
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
      return ts_language_field_id_for_name(
          language_, name.c_str(), name.size());
    }
    void skip_concrete_nodes() {
      while (not ts_node_is_named(ts_tree_cursor_current_node(&cursor_)) and
             (to_sibling() or to_child())) { /* side-effects in loop head */
      };
    }

    bool to_child() {
      if (ts_tree_cursor_goto_first_child(&cursor_)) {
        skip_concrete_nodes();
        return true;
      }
      return false;
    }
    bool to_sibling() {
      if (ts_tree_cursor_goto_next_sibling(&cursor_)) {
        skip_concrete_nodes();
        return true;
      }
      return false;
    }
    bool to_parent() { return ts_tree_cursor_goto_parent(&cursor_); }

    TSSymbol symbol() {
      return ts_node_symbol(ts_tree_cursor_current_node(&cursor_));
    }
    bool at(const TSSymbol Symbols::*symbol) {
      return ts_node_symbol(ts_tree_cursor_current_node(&cursor_)) ==
          symbols_.*symbol;
    }
    bool at(const TSFieldId Fields::*field) {
      return ts_tree_cursor_current_field_id(&cursor_) == fields_.*field;
    }
    bool at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) {
      return at(field) and at(symbol);
    }
    unique_ptr<char> s_expr() {
      return unique_ptr<char>{
          ts_node_string(ts_tree_cursor_current_node(&cursor_))};
    }
    std::string type() {
      return ts_node_type(ts_tree_cursor_current_node(&cursor_));
    }
    void show_field() {
      cout << ts_tree_cursor_current_field_name(&cursor_) << "\n";
    }
    void show() { cout << s_expr() << "\n"; }
    string_view text() {
      auto node = ts_tree_cursor_current_node(&cursor_);
      return {
          std::begin(program_) + ts_node_start_byte(node),
          std::begin(program_) + ts_node_end_byte(node)};
    }

    [[nodiscard]] FirstOrderGraph parse() && {
      FirstOrderGraph graph{string{program_}};

      assert(at(&Symbols::module));
      to_child();
      assert(at(&Symbols::expression_statement));
      to_child();
      assert(at(&Symbols::assignment));
      to_child();
      assert(at(&Fields::left, &Symbols::identifier));
      VarIdx lhs{graph.var_name_to_idx(text())};
      to_sibling();
      assert(at(&Fields::right));
      string_view rhs{text()};

      if (at(&Symbols::identifier)) {
        graph.insert(LoadVar{.lhs = lhs, .rhs = graph.var_name_to_idx(rhs)});
      } else if (at(&Symbols::string) or at(&Symbols::integer)) {
        graph.insert(LoadText{.lhs = lhs, .text_literal = rhs});
      } else if (at(&Symbols::dictionary)) {
        graph.insert(
            LoadRecord{.lhs = lhs, .record_literal = parse_record_literal()});
      } else {
        throw std::domain_error(
            format("assigning ({} {}) not implemented", type(), rhs));
      }

      return graph;
    }

    RecordLiteral parse_record_literal() {
      RecordLiteral record_literal{};
      assert(at(&Symbols::dictionary));
      to_child();
      while (at(&Symbols::pair)) {
        to_child();
        assert(at(&Fields::key));
        record_literal.emplace_back(text());
        to_parent();
        if (not to_sibling()) { break; }
      }

      return record_literal;
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

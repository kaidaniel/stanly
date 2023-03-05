#include "firstorder-lang.h"
#include "stanly-api.h"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <numeric>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tree_sitter/api.h>
#include <variant>
//#include <tree_sitter/parser.h>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <functional>
#include <vector>

extern "C" {
TSLanguage *tree_sitter_python(void);
}

namespace stanly {

using std::string;
using std::unique_ptr;
using str = const std::string &;
using fmt::format;
using stanly::metaprogramming::rebind_t;
using std::begin;
using std::cout;
using std::end;
using std::function;
using std::string_view;
using std::transform_reduce;

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
      TSSymbol list;
      TSSymbol set;
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
              .list = symbol("list"),
              .set = symbol("set"),
          }),
          fields_({
              .left = field("left"),
              .right = field("right"),
              .key = field("key"),
              .value = field("value"),
          }) {
      assert(at(&Symbols::module));
      to_child();
    }
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
    bool skip_concrete_nodes() {
      while (not ts_node_is_named(node()) and
             (to_sibling() or to_child())) { /* side-effects in loop head */
      };
      return true;
    }

    bool to_child() {
      return ts_tree_cursor_goto_first_child(&cursor_) ? skip_concrete_nodes()
                                                       : false;
    }
    bool to_sibling() {
      return ts_tree_cursor_goto_next_sibling(&cursor_) ? skip_concrete_nodes()
                                                        : false;
    }
    bool to_parent() { return ts_tree_cursor_goto_parent(&cursor_); }
    TSNode node() { return node(); }
    TSSymbol symbol() { return ts_node_symbol(node()); }
    bool at(const TSSymbol Symbols::*symbol) {
      return ts_node_symbol(node()) == symbols_.*symbol;
    }
    bool at(const TSFieldId Fields::*field) {
      return ts_tree_cursor_current_field_id(&cursor_) == fields_.*field;
    }
    bool at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) {
      return at(field) and at(symbol);
    }
    auto s_expr() { return unique_ptr<char>{ts_node_string(node())}; }
    std::string type() { return ts_node_type(node()); }
    void show() { cout << s_expr() << "\n"; }
    string_view text() {
      return {
          std::begin(program_) + ts_node_start_byte(node()),
          std::begin(program_) + ts_node_end_byte(node())};
    }

    rebind_t<std::variant, FirstOrderSyntaxNode> next_node() {
      assert(at(&Symbols::expression_statement));
      to_child();
      assert(at(&Symbols::assignment));
      to_child();
      assert(at(&Fields::left, &Symbols::identifier));
      Var left{text()};
      to_sibling();
      assert(at(&Fields::right));
      string_view right{text()};

      if (at(&Symbols::identifier)) { return LoadVar{left, right}; }
      if (at(&Symbols::string)) { return LoadText{left, right}; }
      if (at(&Symbols::integer)) { return LoadText{left, right}; }
      if (at(&Symbols::dictionary)) { return LoadRecord{left, record()}; }
      if (at(&Symbols::set)) { return LoadRecord{left, record()}; }
      if (at(&Symbols::list)) { return LoadTop{left, right}; }
      throw std::domain_error(
          format("assigning ({} {}) not implemented", type(), right));
    }

    RecordLiteral record() {
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
    return FirstOrderGraph{string{program}};
  }
} // namespace treesitter

FirstOrderGraph::FirstOrderGraph(string_view program)
    : program_(string{program}) {
  treesitter::Parser{program_}.parse(*this);
}

Graph parse_firstorder(string_view program) {
  return Graph{treesitter::parse_firstorder, program};
}
} // namespace stanly
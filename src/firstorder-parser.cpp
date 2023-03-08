#include "firstorder-lang.h"
#include "iterator.h"
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

namespace stanly::parser {

using std::string;
using std::unique_ptr;
using str = const std::string &;
using fmt::format;
using stanly::metaprogramming::rebind_t;
using std::make_unique;
using std::string_view;
using iterator::inpt_range;

namespace treesitter { // every use of tree-sitter in this namespace
  class Parser;
  class Parser : public inpt_range<Syntax> {
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
    friend void parse_firstorder(string_view program);
    explicit Parser(string_view program)
        : inpt_range(&Parser::next_node, &Parser::is_done),
          program_(program),
          language_(tree_sitter_python()),
          parser_(ts_parser_new()),
          tree_([&] {
            ts_parser_set_language(parser_, language_);
            return ts_parser_parse_string(
                parser_, nullptr, program_.begin(), program_.size());
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
    [[nodiscard]] TSNode node() const {
      return ts_tree_cursor_current_node(&cursor_);
    }
    [[nodiscard]] TSSymbol symbol() const { return ts_node_symbol(node()); }
    bool at(const TSSymbol Symbols::*symbol) {
      return ts_node_symbol(node()) == symbols_.*symbol;
    }
    bool at(const TSFieldId Fields::*field) {
      return ts_tree_cursor_current_field_id(&cursor_) == fields_.*field;
    }
    bool at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) {
      return at(field) and at(symbol);
    }
    [[nodiscard]] auto s_expr() const {
      return unique_ptr<char>{ts_node_string(node())};
    }
    [[nodiscard]] string type() const { return ts_node_type(node()); }
    string_view text() {
      return {
          std::begin(program_) + ts_node_start_byte(node()),
          std::begin(program_) + ts_node_end_byte(node())};
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

    Syntax next_node() {
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
    [[nodiscard]] bool is_done() const {
      return ts_node_is_null(ts_node_next_sibling(node())) and
          ts_node_child_count(node()) == 0;
    };
  };

} // namespace treesitter

std::unique_ptr<inpt_range<Syntax>> parse_firstorder(string_view program) {
  return make_unique<treesitter::Parser>(program); // TODO: will this cause slicing? how will Parser's destructor be called?
}

} // namespace stanly::parser
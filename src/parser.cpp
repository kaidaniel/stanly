#include <tree_sitter/api.h>

#include <cassert>
#include <cstring>
#include <stdexcept>
#include <string>
#include <string_view>
#include <variant>
#include "syntax.h"
#include "firstorder-syntax.h"
#include "iterator.h"
#include <fmt/core.h>
#include <fmt/ranges.h>

#include <concepts>
#include <functional>
#include <vector>

extern "C" {
TSLanguage *tree_sitter_python(void);
}

namespace stanly {
using std::same_as;
using std::string;
using std::unique_ptr;
using fmt::format;
using std::string_view;
using std::vector;
class parser {
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
  explicit parser(string_view program)
      : program_(program),
        language_(tree_sitter_python()),
        parser_(ts_parser_new()),
        tree_([&] {
          ts_parser_set_language(parser_, language_);
          return ts_parser_parse_string(parser_, nullptr, program_.begin(), program_.size());
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
  ~parser() {
    ts_tree_delete(tree_);
    ts_parser_delete(parser_);
  }
  parser(const parser &) = delete;
  parser operator=(const parser &) = delete;
  parser(parser &&) = delete;
  parser operator=(parser &&) = delete;

  [[nodiscard]] const TSNode &root() const { return root_; }
  [[nodiscard]] TSSymbol symbol(const string& name) const {
    return ts_language_symbol_for_name(language_, name.c_str(), name.length(), true);
  }
  [[nodiscard]] TSFieldId field(const string& name) const {
    return ts_language_field_id_for_name(language_, name.c_str(), name.size());
  }
  bool skip_concrete_nodes() {
    while (not ts_node_is_named(node()) and
           (to_sibling() or to_child())) { /* side-effects in loop head */
    };
    return true;
  }

  bool to_child() {
    return ts_tree_cursor_goto_first_child(&cursor_) ? skip_concrete_nodes() : false;
  }
  bool to_sibling() {
    return ts_tree_cursor_goto_next_sibling(&cursor_) ? skip_concrete_nodes() : false;
  }
  bool to_parent() { return ts_tree_cursor_goto_parent(&cursor_); }
  [[nodiscard]] TSNode node() const { return ts_tree_cursor_current_node(&cursor_); }
  [[nodiscard]] TSSymbol symbol() const { return ts_node_symbol(node()); }
  bool at(const TSSymbol Symbols::*symbol) { return ts_node_symbol(node()) == symbols_.*symbol; }
  bool at(const TSFieldId Fields::*field) {
    return ts_tree_cursor_current_field_id(&cursor_) == fields_.*field;
  }
  bool at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) {
    return at(field) and at(symbol);
  }
  [[nodiscard]] auto s_expr() const { return unique_ptr<char>{ts_node_string(node())}; }
  [[nodiscard]] string type() const { return ts_node_type(node()); }
  string_view text() {
    return {std::begin(program_) + ts_node_start_byte(node()),
            std::begin(program_) + ts_node_end_byte(node())};
  }

  vector<string_view> record() {
    vector<string_view> record_v{};
    assert(at(&Symbols::dictionary));
    to_child();
    while (at(&Symbols::pair)) {
      to_child();
      assert(at(&Fields::key));
      record_v.emplace_back(text());
      to_parent();
      if (not to_sibling()) {
        break;
      }
    }

    return record_v;
  }

  template <syntax S>
    requires same_as<typename S::repr, string_view>
  typename S::node next_node();

  [[nodiscard]] bool is_done() const {
    return ts_node_is_null(ts_node_next_sibling(node())) and ts_node_child_count(node()) == 0;
  };
};
}

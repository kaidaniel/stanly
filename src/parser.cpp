#include "parser.h"

#include <tree_sitter/api.h>

#include <cassert>
#include <functional>
#include <memory>
#include <vector>

extern "C" {
TSLanguage *tree_sitter_python(void);
}

namespace stanly {
using std::same_as;
using std::string;
using std::string_view;
using std::vector;
parser::parser(string_view program)
    : program_(program),
      language_(tree_sitter_python()),
      parser_(ts_parser_new()),
      tree_([this] {
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
          .subscript = symbol("subscript"),
      }),
      fields_({
          .left = field("left"),
          .right = field("right"),
          .key = field("key"),
          .value = field("value"),
          .subscript = field("subscript"),
      }) {
  assert(at(&Symbols::module));
  to_child();
}
parser::~parser() {
  ts_tree_delete(tree_);
  ts_parser_delete(parser_);
}

[[nodiscard]] const TSNode &parser::root() const { return root_; }
[[nodiscard]] TSSymbol parser::symbol(const string &name) const {
  return ts_language_symbol_for_name(language_, name.c_str(), name.length(), true);
}
[[nodiscard]] TSFieldId parser::field(const string &name) const {
  return ts_language_field_id_for_name(language_, name.c_str(), name.size());
}
bool parser::skip_concrete_nodes() {
  while (not ts_node_is_named(node()) and
         (to_sibling() or to_child())) { /* side effects in loop head */
  };
  return true;
}

bool parser::to_child() {
  return ts_tree_cursor_goto_first_child(&cursor_) ? skip_concrete_nodes() : false;
}
bool parser::to_sibling() {
  return ts_tree_cursor_goto_next_sibling(&cursor_) ? skip_concrete_nodes() : false;
}
bool parser::to_parent() { return ts_tree_cursor_goto_parent(&cursor_); }
[[nodiscard]] TSNode parser::node() const { return ts_tree_cursor_current_node(&cursor_); }
[[nodiscard]] TSSymbol parser::symbol() const { return ts_node_symbol(node()); }
bool parser::at(const TSSymbol Symbols::*symbol) {
  return ts_node_symbol(node()) == symbols_.*symbol;
}
bool parser::at(const TSFieldId Fields::*field) {
  return ts_tree_cursor_current_field_id(&cursor_) == fields_.*field;
}
bool parser::at(const TSFieldId Fields::*field, const TSSymbol Symbols::*symbol) {
  return at(field) and at(symbol);
}
[[nodiscard]] string parser::type() const { return ts_node_type(node()); }
[[nodiscard]] std::unique_ptr<char> parser::s_expr() const {
  return std::unique_ptr<char>{ts_node_string(node())};
}
string_view parser::text() {
  return {std::begin(program_) + ts_node_start_byte(node()),
          std::begin(program_) + ts_node_end_byte(node())};
}

vector<string_view> parser::record() {
  vector<string_view> record_v{};
  assert(at(&Symbols::dictionary));
  to_child();
  while (at(&Symbols::pair)) {
    to_child();
    assert(at(&Fields::key));
    record_v.emplace_back(text());
    to_parent();
    if (not to_sibling()) { break; }
  }

  return record_v;
}
[[nodiscard]] TSTreeCursor parser::copy_cursor() const { return ts_tree_cursor_copy(&cursor_); }
void parser::set_cursor(TSTreeCursor cursor) { cursor_ = cursor; };
}  // namespace stanly

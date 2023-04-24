#include <string_view>

#include "parse.h"
#include "tree_sitter/api.h"
namespace stanly {
auto lookup_symbol(std::string_view name) -> TSSymbol {
  return ts_language_symbol_for_name(tree_sitter_python(), name.data(), name.length(), true);
}
auto lookup_field(std::string_view name) -> TSFieldId {
  return ts_language_field_id_for_name(tree_sitter_python(), name.data(), name.size());
}

cursor::cursor(TSTreeCursor* crsr, std::string_view program) : cursor_{crsr}, program_{program} {};
auto cursor::node() -> TSNode { return ts_tree_cursor_current_node(cursor_); }
auto cursor::symbol() -> TSSymbol { return ts_node_symbol(node()); };
auto cursor::field() -> TSFieldId { return ts_tree_cursor_current_field_id(cursor_); };
bool cursor::goto_child() { return ts_tree_cursor_goto_first_child(cursor_); };
bool cursor::goto_sibling() { return ts_tree_cursor_goto_next_sibling(cursor_); };
bool cursor::goto_parent() { return ts_tree_cursor_goto_parent(cursor_); };
auto cursor::text(const TSNode node) -> std::string_view {
  return {program_.begin() + ts_node_start_byte(node), program_.begin() + ts_node_end_byte(node)};
};
auto cursor::text() -> std::string_view { return text(node()); }
auto cursor::text(TSFieldId field) -> std::string_view {
  return text(ts_node_child_by_field_id(node(), field));
}

parser::parser(std::string_view program)
    : parser_{[] {
        auto* p = ts_parser_new();
        ts_parser_set_language(p, tree_sitter_python());
        return p;
      }()},
      tree_{ts_parser_parse_string(parser_.get(), nullptr, program.begin(), program.size())},
      cursor_{ts_tree_cursor_new(ts_node_named_child(ts_tree_root_node(tree_.get()), 0))} {}
TSTreeCursor* parser::cursor() { return &cursor_; }
parser::~parser() { ts_tree_cursor_delete(cursor()); };
}  // namespace stanly
#include "cursor.hpp"

#include "handle.h"
#include "string-index.h"
#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly::parser {
struct cursor {
  cursor(std::string_view, string_index*);
  cursor(const cursor&) = delete;
  cursor(cursor&&) = delete;
  cursor& operator=(const cursor&) = delete;
  cursor& operator=(const cursor&&) = delete;
  ~cursor();

  friend handle current_handle(cursor&);
  friend symbol current_symbol(cursor&);
  friend std::optional<field> current_field(cursor&);
  friend bool goto_child(cursor&);
  friend bool goto_sibling(cursor&);
  friend void goto_parent(cursor&);

 private:
  std::function<void()> destroy;
  TSTreeCursor ts_cursor{};
  string_index* idx;
  std::string_view program;
};

cursor::cursor(std::string_view p, string_index* si) : program(p), idx(si) {
  auto* parser = ts_parser_new();
  ts_parser_set_language(parser, tree_sitter_python());
  auto* tree = ts_parser_parse_string(parser, nullptr, program.begin(), program.size());
  ts_cursor = ts_tree_cursor_new(ts_tree_root_node(tree));
  destroy = [parser, tree, ts_cursor_addr = &ts_cursor]() mutable {
    ts_parser_delete(parser);
    ts_tree_delete(tree);
    ts_tree_cursor_delete(ts_cursor_addr);
  };
}
cursor::~cursor() { destroy(); }

handle
current_handle(cursor& c) {
  const auto node = ts_tree_cursor_current_node(&c.ts_cursor);
  const auto start = ts_node_start_byte(node);
  const auto end = ts_node_end_byte(node);
  return c.idx->insert(c.program.substr(start, end - start));
};
symbol
current_symbol(cursor& c) {
  return static_cast<symbol>(ts_node_symbol(ts_tree_cursor_current_node(&c.ts_cursor)));
};
std::optional<field>
current_field(cursor& c) {
  auto field_id = ts_tree_cursor_current_field_id(&c.ts_cursor);
  if (field_id != 0) { return std::nullopt; }
  return static_cast<field>(field_id);
};

bool
goto_child(cursor& c) {
  if (ts_tree_cursor_goto_first_child(&c.ts_cursor)) {
    do {
      if (ts_node_is_named(ts_tree_cursor_current_node(&c.ts_cursor))) { return true; }
    } while (ts_tree_cursor_goto_next_sibling(&c.ts_cursor));
  }
  return false;
};
bool
goto_sibling(cursor& c) {
  while (ts_tree_cursor_goto_next_sibling(&c.ts_cursor)) {
    if (ts_node_is_named(ts_tree_cursor_current_node(&c.ts_cursor))) { return true; }
  }
  return false;
};
void
goto_parent(cursor& c) {
  ts_tree_cursor_goto_parent(&c.ts_cursor);
}

}  // namespace stanly::parser
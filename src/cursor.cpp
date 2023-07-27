#include "cursor.h"

#include <cstdint>
#include <functional>
#include <optional>
#include <string_view>

#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {
struct cursor {
  cursor(std::string_view);
  cursor(const cursor&) = delete;
  cursor(cursor&&) = delete;
  cursor& operator=(const cursor&) = delete;
  cursor& operator=(const cursor&&) = delete;
  ~cursor();

  friend std::string_view current_text(cursor&);
  friend uint16_t current_symbol(cursor&);
  friend std::optional<uint16_t> current_field(cursor&);
  friend bool goto_child(cursor&);
  friend bool goto_sibling(cursor&);
  friend void goto_parent(cursor&);

 private:
  std::function<void()> destroy;
  TSTreeCursor ts_cursor{};
  std::string_view program;
};

cursor::cursor(std::string_view p) : program(p) {
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

std::string_view
current_text(cursor& c) {
  const auto node = ts_tree_cursor_current_node(&c.ts_cursor);
  const auto start = ts_node_start_byte(node);
  const auto end = ts_node_end_byte(node);
  return (c.program.substr(start, end - start));
};
uint16_t
current_symbol(cursor& c) {
  return static_cast<uint16_t>(ts_node_symbol(ts_tree_cursor_current_node(&c.ts_cursor)));
};
std::optional<uint16_t>
current_field(cursor& c) {
  auto field_id = ts_tree_cursor_current_field_id(&c.ts_cursor);
  if (field_id != 0) { return std::nullopt; }
  return static_cast<uint16_t>(field_id);
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
}  // namespace stanly
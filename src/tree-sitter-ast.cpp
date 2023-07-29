#include <functional>
#include <optional>
#include <string_view>

#include "stanly-concepts.h"  // IWYU pragma: keep
#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {
  struct tree_sitter_ast {
      struct treesitter_ast_node {
      std::optional<int> field;
      int symbol;
      std::string_view text;
  };
  tree_sitter_ast(std::string_view, TSLanguage*);
  tree_sitter_ast(const tree_sitter_ast&) = delete;
  tree_sitter_ast(tree_sitter_ast&&) = delete;
  tree_sitter_ast& operator=(const tree_sitter_ast&) = delete;
  tree_sitter_ast& operator=(const tree_sitter_ast&&) = delete;
  ~tree_sitter_ast();

  friend treesitter_ast_node value(tree_sitter_ast&);
  friend bool goto_child(tree_sitter_ast&);
  friend bool goto_sibling(tree_sitter_ast&);
  friend bool goto_parent(tree_sitter_ast&);

 private:
  std::function<void()> destroy;
  TSTreeCursor ts_cursor{};
  std::string_view program;
};

tree_sitter_ast::tree_sitter_ast(std::string_view p, TSLanguage* lang) : program(p) {
  auto* parser = ts_parser_new();
  ts_parser_set_language(parser, lang);
  auto* tree = ts_parser_parse_string(parser, nullptr, program.begin(), program.size());
  ts_cursor = ts_tree_cursor_new(ts_tree_root_node(tree));
  destroy = [parser, tree, ts_cursor_addr = &ts_cursor]() mutable {
    ts_parser_delete(parser);
    ts_tree_delete(tree);
    ts_tree_cursor_delete(ts_cursor_addr);
  };
}
tree_sitter_ast::~tree_sitter_ast() { destroy(); }

bool
goto_child(tree_sitter_ast& c) {
  if (ts_tree_cursor_goto_first_child(&c.ts_cursor)) {
    do {
      if (ts_node_is_named(ts_tree_cursor_current_node(&c.ts_cursor))) { return true; }
    } while (ts_tree_cursor_goto_next_sibling(&c.ts_cursor));
  }
  return false;
};
bool
goto_sibling(tree_sitter_ast& c) {
  while (ts_tree_cursor_goto_next_sibling(&c.ts_cursor)) {
    if (ts_node_is_named(ts_tree_cursor_current_node(&c.ts_cursor))) { return true; }
  }
  return false;
};
bool
goto_parent(tree_sitter_ast& c) {
  return ts_tree_cursor_goto_parent(&c.ts_cursor);
}

tree_sitter_ast::treesitter_ast_node
value(tree_sitter_ast& c) {
  const auto node = ts_tree_cursor_current_node(&c.ts_cursor);
  const auto start = ts_node_start_byte(node);
  const auto end = ts_node_end_byte(node);
  const auto field_id = ts_tree_cursor_current_field_id(&c.ts_cursor);
  return {
      field_id == 0 ? std::nullopt : std::optional{field_id},
      ts_node_symbol(ts_tree_cursor_current_node(&c.ts_cursor)),
      std::string_view{c.program.substr(start, end - start)}};
}

struct python_ast : public tree_sitter_ast { python_ast(std::string_view sv) : tree_sitter_ast(sv, tree_sitter_python()){}};

static_assert(tree<python_ast, python_ast::treesitter_ast_node>);
}  // namespace stanly
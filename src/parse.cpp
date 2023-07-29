
#include "parse.h"

#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "assemble.h"
#include "string-index.h"
#include "syntax.h"
#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {

struct tree_sitter_ast_node {
  std::optional<int> field;
  int symbol;
  std::string_view text;
};

struct tree_sitter_ast {
  tree_sitter_ast(std::string_view p, TSLanguage* lang) : program(p) {
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
  ~tree_sitter_ast() { destroy(); }
  tree_sitter_ast(const tree_sitter_ast&) = delete;
  tree_sitter_ast(tree_sitter_ast&&) = delete;
  tree_sitter_ast& operator=(const tree_sitter_ast&) = delete;
  tree_sitter_ast& operator=(const tree_sitter_ast&&) = delete;

  TSTreeCursor ts_cursor{};
  std::string_view program;
  using tree_node = tree_sitter_ast_node;

 private:
  std::function<void()> destroy;
};
bool
goto_child(tree_sitter_ast& t) {
  if (ts_tree_cursor_goto_first_child(&t.ts_cursor)) {
    do {
      if (ts_node_is_named(ts_tree_cursor_current_node(&t.ts_cursor))) { return true; }
    } while (ts_tree_cursor_goto_next_sibling(&t.ts_cursor));
  }
  return false;
};
bool
goto_sibling(tree_sitter_ast& t) {
  while (ts_tree_cursor_goto_next_sibling(&t.ts_cursor)) {
    if (ts_node_is_named(ts_tree_cursor_current_node(&t.ts_cursor))) { return true; }
  }
  return false;
};
bool
goto_parent(tree_sitter_ast& t) {
  return ts_tree_cursor_goto_parent(&t.ts_cursor);
}

tree_sitter_ast_node
value(tree_sitter_ast& t) {
  const auto node = ts_tree_cursor_current_node(&t.ts_cursor);
  const auto start = ts_node_start_byte(node);
  const auto end = ts_node_end_byte(node);
  const auto field_id = ts_tree_cursor_current_field_id(&t.ts_cursor);
  return {
      field_id == 0 ? std::nullopt : std::optional{field_id},
      ts_node_symbol(ts_tree_cursor_current_node(&t.ts_cursor)),
      std::string_view{t.program.substr(start, end - start)}};
}

template <arg_of<node::variant> T>
void
construct(cfg& c, std::convertible_to<std::string_view> auto... args) {
  c.basic_blocks.back().nodes.emplace_back(c.idx.make<T>(args...));
}
template <arg_of<decltype(basic_block::next)> T>
void
construct(cfg& c, std::convertible_to<std::string_view> auto... args) {
  c.basic_blocks.emplace_back(c.idx.make<T>(args...), std::vector<node>{});
};
inline std::string_view
add_string_to_index(cfg& c, std::string&& str) {
  return c.idx.add_string_to_index(std::move(str));
}

struct python_ast : public tree_sitter_ast {
  python_ast(std::string_view sv) : tree_sitter_ast(sv, tree_sitter_python()) {}
};

inline auto
node_kind(const tree_sitter_ast_node& n) {
  return n.symbol;
}

static_assert(assembler_c<cfg>);
static_assert(tree_c<python_ast>);

std::unique_ptr<cfg>
parse(std::string&& source, lang_tag<lang::python>) {
  auto assembler = std::make_unique<cfg>();
  auto parse_tree = python_ast(add_string_to_index(*assembler, std::move(source)));
  assemble(parse_tree, *assembler);
  return assembler;
}

}  // namespace stanly
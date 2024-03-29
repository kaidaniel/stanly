
#include "parse.h"

#include <__ranges/filter_view.h>

#include <algorithm>
#include <cstddef>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "parser-symbols.h"
#include "stanly-assert.h"
#include "string-index.h"
#include "syntax.h"
#include "to_tpl.h"
#include "tree.h"
#include "tree_sitter/api.h"

extern "C" {
TSLanguage* tree_sitter_python(void);
}

namespace stanly {
namespace rg = std::ranges;

struct ast_node {
  std::optional<std::size_t> field;
  std::size_t symbol;
  std::string_view text;
};

struct tree_sitter_ast {
  tree_sitter_ast(std::string_view p, TSLanguage* lang = tree_sitter_python()) : program(p) {
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
  using tree_node = ast_node;

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

ast_node
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

inline auto
node_kind(const ast_node& n) {
  return n.symbol;
}

template <class T>
// clang-format off
concept assembler_c = std::default_initializable<T> && requires(std::string source, T t) {
      std::visit([]<class IrNode>(IrNode n) {std::apply([](auto... args) {
          T t; construct<IrNode>(t, ((void)args, std::string_view{})...);},to_tpl(n));}, t.basic_blocks[0].next);
      { add_string_to_index(t, std::move(source)) } -> std::same_as<std::string_view>;
};
// clang-format on

static_assert(assembler_c<cfg>);
static_assert(tree_c<tree_sitter_ast>);

namespace {
template <symbol s>
using stag = tag<static_cast<std::size_t>(s)>;

template <class T, assembler_c Assembler, class Node = read>
struct make {
  Assembler& assembler;
  std::span<T> children;
  T* arg;
  Node node{};
  make(Assembler& a, std::span<T> c, T* arg) : assembler{a}, children(c), arg(arg) {}
  template <class N>
  make(Assembler& a, std::span<T> c, T* arg, N node)
      : assembler{a}, children(c), arg(arg), node(node) {}

  std::string_view
  text(field f) {
    return find(f);
  }
  std::string_view
  text(symbol s) {
    return find(s);
  }
  std::string_view
  text(T* t) {
    return t->text;
  }
  std::string_view
  text(const T& t) {
    return t.text;
  }
  std::string_view
  text(std::string_view sv) {
    return sv;
  }
  struct node_tag {};

  auto
  find(node_tag, field fld) {
    auto ret = rg::find(children, std::optional{static_cast<std::size_t>(fld)}, &ast_node::field);
    stanly_assert(ret != children.end());
    return *ret;
  }

  std::string_view
  find(const field fld) {
    return find(node_tag{}, fld).text;
  }
  template <class N>
  make<T, Assembler, N>
  operator()(N node) {
    return make<T, Assembler, N>{assembler, children, arg, node};
  }
  template <class A, class B, class C>
  make&
  operator()(const A& a, const B& b, const C& c) {
    construct<Node>(assembler, text(a), text(b), text(c));
    return *this;
  }
  template <class A, class B>
  make&
  operator()(const A& a, const B& b) {
    construct<Node>(assembler, text(a), text(b));
    return *this;
  }
};
using enum field;
using enum symbol;
}  // namespace

// clang-format off

void visit_tree_node_default(std::size_t, assembler_c auto& a, ast_node* node, std::span<ast_node> children){make{a, children, node}
  (top())(node, "symbol not implemented");
}

void visit_tree_node(stag<symbol::sym_false>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { make{a, children, node}
  (lit())(node, "bool", "false"); }

void visit_tree_node(stag<symbol::sym_true>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { make{a, children, node}
  (lit())(node, "bool", "true"); }

void visit_tree_node(stag<symbol::sym_none>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { make{a, children, node}
  (lit())(node, "None", "None"); }

void visit_tree_node(stag<symbol::sym_integer>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { make{a, children, node}
  (lit())(node, "int", node); }

void visit_tree_node(stag<symbol::sym_string>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { 
  auto m = make{a, children, node};
  auto once = [](auto&& f) { constinit static bool can_run = true; if(can_run) { f(); can_run = false; }  };
  if(children.empty()){
    m(lit())(node, "str", node); 
  } else {
    for(auto& child : children) { if(static_cast<symbol>(child.field.value_or(0)) == symbol::sym_interpolation){ 
      once([&]{m
        (alloc()) (node, "interpolated_string")
        (append())(node, node); 
      });
      m(append())(node, child);
    }
   }
  }
}

void visit_tree_node(stag<symbol::sym_assignment>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { make{a, children, node}
  (ref())(fld_left, fld_right);}

void visit_tree_node(stag<symbol::sym_named_expression>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) {make{a, children, node}
  (ref())(fld_name, fld_value)
  (ref())(node, fld_value);
}

void
visit_tree_node(stag<symbol::sym_dictionary>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { 
  auto m = make{a, children, node};
  m(alloc())(node, "dict");
  for(auto child : children) { m(merge())(node, node, child); }
}

void
visit_tree_node(stag<symbol::sym_pair>, assembler_c auto& a, ast_node* node, std::span<ast_node> children){
  make{a, children, node}
  (alloc())(node, "dict")
  (read())(fld_key, node, fld_key)
  (write())(fld_key, fld_value);
}

void
visit_tree_node(stag<symbol::sym_augmented_assignment>, assembler_c auto& a, ast_node* node, std::span<ast_node> children) { make{a, children, node}
  (alloc()) (node, "args")
  (append())(node, fld_left)
  (append())(node, fld_right)
  (dcall()) (fld_left, fld_operator, node)
  (ref())   (fld_left, fld_right);
}

// TODO: make sure @attributes is always present
void
visit_tree_node(stag<symbol::sym_attribute>, assembler_c auto& a, ast_node* node, std::span<ast_node> c) { make{a, c, node}
  (read())(node, fld_object, "@attributes")
  (read())(node, node,        fld_attribute);
}
// TODO: make sure @subscripts is always present
void
visit_tree_node(stag<symbol::sym_subscript>, assembler_c auto& a, ast_node* node, std::span<ast_node> c) { make{a, c, node}
  (read())(node, fld_value, "@subscripts")
  (read())(node, node,       fld_subscript);
}
// clang-format on

std::unique_ptr<cfg>
parse(std::string&& source, lang_tag<lang::python>) {
  auto assembler = std::make_unique<cfg>();
  auto parse_tree = tree_sitter_ast(add_string_to_index(*assembler, std::move(source)));
  visit_tree_nodes<cfg>(parse_tree, *assembler);
  return assembler;
}

std::vector<node>
parse(std::string&& source) {
  // TODO remove this after removing the old parser
  return parse(std::move(source), lang_tag<lang::python>{})->basic_blocks.back().nodes;
}

}  // namespace stanly
#pragma once

#include <array>
#include <cstddef>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "stanly-assert.h"
#include "to_tpl.h"

namespace stanly {
template <class T>
// clang-format off
concept assembler_c = std::default_initializable<T> && requires(std::string source, T t) {
      std::visit([]<class IrNode>(IrNode n) {std::apply([](auto... args) {
          T t; construct<IrNode>(t, ((void)args, std::string_view{})...);},to_tpl(n));}, t.basic_blocks[0].next);
      { add_string_to_index(t, std::move(source)) } -> std::same_as<std::string_view>;
};
// clang-format on

template <class T>
concept tree_c = std::constructible_from<T, std::string_view> && requires(T t) {
  typename T::tree_node;
  { goto_child(t) ? 1 : 0 };
  { goto_sibling(t) ? 1 : 0 };
  { goto_parent(t) ? 1 : 0 };
  { value(t) } -> std::same_as<typename T::tree_node>;
};

template <class T>
concept compiler_c =
    std::constructible_from<T, std::string&&> && requires(T t, decltype(t.tree)::tree_node n) {
      requires assembler_c<decltype(t.assembler)>;
      requires tree_c<decltype(t.tree)>;
      { node_kind(n) } -> std::integral;
    };

template <std::size_t N>
struct tag {};

template <class TreeNode, assembler_c Assembler>
struct visit_tree_node_functions {
  template <std::size_t N>
  static void
  function(Assembler& program, TreeNode* node, std::span<TreeNode> children) {
    if constexpr (requires { parse_symbol(program, node, children, tag<N>{}); }) {
      visit_tree_node(program, node, children, tag<N>{});
    } else if constexpr (requires { parse_symbol(program, node, tag<N>{}); }) {
      visit_tree_node(program, node, tag<N>{});
    }
  }
};

template <std::size_t N, class FunctionPointers>
constexpr auto jump_table = [] {
  return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
    FunctionPointers f{};
    std::array<std::decay_t<decltype(f.template function<0>)>, N> arr{};
    ((arr[Is] = &f.template function<Is>), ...);
    return [=](std::size_t idx) {
      stanly_assert(idx <= N, std::format("jump_table index out of bounds: {} > {}", idx, N));
      stanly_assert(idx >= 0, std::format("jump table index negative: {} (len: {})", idx, N));
      return arr[idx];
    };
  }(std::make_index_sequence<N>{});
}();

// https://en.wikipedia.org/wiki/Tree_traversal#/media/File:Sorted_binary_tree_ALL_RGB.svg
//
//     pre-order                post-order
//       '1                        ^13             ': called as child-of-previous
//      / | \                     /  | \           *: called as sibling-of-previous
//    '2 *7  *8                 ^5  *6  ^12        ^: called as parent-of-previous
//    /     / | \               /      / | \
//  '3    '9 *10 *13          ^4     '7  ^10 *11
//  /|\       |              / | \       |
// 4 5 6     '11           '1 *2 *3      ^9
// ' * *      |                          |
//           '12                        '8
template <tree_c Tree, class Pre, class Post>
  requires requires(Pre f, Post g, Tree::tree_node n) {
    f(std::move(n));
    g(std::move(n));
  }
void
traverse_tree(Tree& tree, Pre&& cc_pre_order, Post&& cc_post_order) {
  while (true) {
    cc_pre_order(value(tree));
    if (goto_child(tree)) { continue; }  // '
    while (true) {
      cc_post_order(value(tree));
      if (goto_sibling(tree)) { break; }   // *
      if (!goto_parent(tree)) { return; }  // ^
    }
  }
}

template <compiler_c Compiler>
decltype(Compiler::assembler)
parse(std::string&& source) {
  using tree_node = typename Compiler::tree_node;
  using fns = visit_tree_node_functions<tree_node, decltype(Compiler::assembler)>;
  Compiler compiler{std::move(source)};
  std::vector<tree_node> children = {};
  std::vector<std::size_t> n_children = {0};
  traverse_tree(
      compiler.tree,
      [&](tree_node&& v) {  // called in pre-order
        children.push_back(std::move(v));
        n_children.back() += 1;
        n_children.push_back(0);
      },
      [&](tree_node&& v) {  // called in post-order
        std::size_t n_args = n_children.back();
        n_children.pop_back();
        stanly_assert(
            children.size() >= (n_args + 1),
            std::format(
                "not enough children to visit node: expected {}, but got only {}", n_args + 1,
                children.size()));
        std::span<tree_node> args = {children.end() - n_args, children.end()};
        auto symbol = node_kind(std::move(v));
        jump_table<JUMP_TABLE_MAX_SIZE, fns>(symbol)(compiler.assembler, args.data() - 1, args);
        children.erase(children.end() - n_args, children.end());
      });
  return compiler.assembler;
};

}  // namespace stanly
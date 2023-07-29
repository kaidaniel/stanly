#pragma once

#include <array>
#include <cstddef>
#include <format>
#include <span>
#include <string_view>
#include <type_traits>
#include <utility>

#include "stanly-assert.h"

namespace stanly {
template <class T>
concept tree_c = std::constructible_from<T, std::string_view> && requires(T t) {
  typename T::tree_node;
  { goto_child(t) ? 1 : 0 };
  { goto_sibling(t) ? 1 : 0 };
  { goto_parent(t) ? 1 : 0 };
  { value(t) } -> std::same_as<typename T::tree_node>;
};

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
    std::forward<Pre>(cc_pre_order)(value(tree));
    if (goto_child(tree)) { continue; }  // '
    while (true) {
      std::forward<Post>(cc_post_order)(value(tree));
      if (goto_sibling(tree)) { break; }   // *
      if (!goto_parent(tree)) { return; }  // ^
    }
  }
}

template <std::size_t N>
struct tag {};

template <class T, class TreeNode>
struct visit_tree_node_functions {
  template <std::size_t N>
  static void
  function(T& t, TreeNode* node, std::span<TreeNode> children) {
    if constexpr (requires { visit_tree_node(t, node, children, tag<N>{}); }) {
      visit_tree_node(t, node, children, tag<N>{});
    } else if constexpr (requires { visit_tree_node(t, node, tag<N>{}); }) {
      visit_tree_node(t, node, tag<N>{});
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
}  // namespace stanly

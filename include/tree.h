#pragma once

#include <array>
#include <cstddef>
#include <format>
#include <span>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

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

template <std::size_t N = JUMP_TABLE_MAX_SIZE, class Values>
  requires requires(Values values) { values.template operator()<0>(); }
constexpr auto
make_array(Values values) {
  using array = std::array<std::decay_t<decltype(values.template operator()<0>())>, N>;
  return [&]<std::size_t... Is>(std::index_sequence<Is...>) -> array {
    array arr{};
    ((arr[Is] = values.template operator()<Is>()), ...);
    return arr;
  }(std::make_index_sequence<N>{});
}

template <std::size_t N>
struct tag {};

template <class State>
constexpr State&
visit_tree_nodes(tree_c auto& parse_tree, State& state) {
  using tree_node = typename std::decay_t<decltype(parse_tree)>::tree_node;
  std::vector<tree_node> children = {};
  std::vector<std::size_t> n_children = {0};
  traverse_tree(
      parse_tree,
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

        constexpr static std::array jump_table = make_array([]<std::size_t N> {
          return +[](State& state, tree_node* node, std::span<tree_node> children) {
            if constexpr (requires { visit_tree_node(tag<N>{}, state, node, children); }) {
              visit_tree_node(tag<N>{}, state, node, children);
            } else if constexpr (requires { visit_tree_node(tag<N>{}, state, node); }) {
              visit_tree_node(tag<N>{}, state, node);
            }
          };
        });
        stanly_assert(
            symbol <= jump_table.size(),
            std::format("jump_table index out of bounds: {} > {}", symbol, jump_table.size()));
        stanly_assert(
            symbol >= 0,
            std::format("jump table index negative: {} (len: {})", symbol, jump_table.size()));
        jump_table[symbol](state, args.data() - 1, args);
        children.erase(children.end() - n_args, children.end());
      });
  return state;
};

}  // namespace stanly

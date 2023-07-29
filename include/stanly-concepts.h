#pragma once

#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "to_tpl.h"

namespace stanly{
template <class T>
// clang-format off
concept assembler = std::default_initializable<T> && requires(T::ir_node obj, std::string source, T t) {
      std::visit([]<class IrNode>(IrNode n) {std::apply([](auto... args) {
          T t; construct<IrNode>(t, ((void)args, std::string_view{})...);},to_tpl(n));}, obj);
      { add_string_to_index(t, std::move(source)) } -> std::same_as<std::string_view>;
};
// clang-format on

template <class T, class TreeNode>
concept tree = std::constructible_from<T, std::string_view> && requires(T t) {
  { goto_child(t) ? 1 : 0 };
  { goto_sibling(t) ? 1 : 0 };
  { goto_parent(t) ? 1 : 0 };
  { value(t) } -> std::same_as<TreeNode>;
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
template <class TreeNode>
void
traverse_tree(
    tree<TreeNode> auto& cursor, std::invocable<TreeNode> auto&& cc_pre_order,
    std::invocable<TreeNode> auto&& cc_post_order) {
  while (true) {
    std::invoke(cc_pre_order, value(cursor));
    if (goto_child(cursor)) { continue; }  // '
    while (true) {
      std::invoke(cc_post_order, value(cursor));
      if (goto_sibling(cursor)) { break; }   // *
      if (!goto_parent(cursor)) { return; }  // ^
    }
  }
}

}
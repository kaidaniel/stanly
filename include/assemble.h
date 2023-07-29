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
#include "tree.h"

namespace stanly {
template <class T>
// clang-format off
concept assembler_c = std::default_initializable<T> && requires(std::string source, T t) {
      std::visit([]<class IrNode>(IrNode n) {std::apply([](auto... args) {
          T t; construct<IrNode>(t, ((void)args, std::string_view{})...);},to_tpl(n));}, t.basic_blocks[0].next);
      { add_string_to_index(t, std::move(source)) } -> std::same_as<std::string_view>;
};
// clang-format on

template <std::size_t N>
struct tag {};

namespace {
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
}  // namespace

void
assemble(tree_c auto& parse_tree, assembler_c auto& assembler) {
  using tree_node = typename std::decay_t<decltype(parse_tree)>::tree_node;
  using fns = visit_tree_node_functions<tree_node, std::decay_t<decltype(assembler)>>;
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
        jump_table<JUMP_TABLE_MAX_SIZE, fns>(symbol)(assembler, args.data() - 1, args);
        children.erase(children.end() - n_args, children.end());
      });
};

}  // namespace stanly
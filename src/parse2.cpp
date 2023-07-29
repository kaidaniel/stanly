#include <array>
#include <cstddef>
#include <format>
#include <span>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "stanly-assert.h"
#include "stanly-concepts.h"  // IWYU pragma: keep (clangd bug with concepts: https://github.com/llvm/llvm-project/issues/60702)

namespace stanly {

template <std::size_t N>
struct tag {};

template <class TreeNode, assembler Assembler>
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
};

}  // namespace stanly
#pragma once

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

void
assemble(tree_c auto& parse_tree, assembler_c auto& assembler) {
  using tree_node = typename std::decay_t<decltype(parse_tree)>::tree_node;
  using fns = visit_tree_node_functions<std::decay_t<decltype(assembler)>, tree_node>;
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
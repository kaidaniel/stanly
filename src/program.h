#pragma once

#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "string-index.h"
#include "syntax.h"

namespace {
template <class T1, class T2>
struct concat_type_args;
template <class... xs, class... ys, template <class...> class T1, template <class...> class T2>
struct concat_type_args<T1<xs...>, T2<ys...>> {
  using type = T1<xs..., ys...>;
};
}  // namespace

namespace stanly {
struct program {
  template <class T>
  friend void construct(program& p, auto... args);
  using objects = concat_type_args<node::variant, std::decay_t<decltype(basic_block::next)>>::type;
  std::vector<node> nodes;
  std::vector<basic_block> basic_blocks;
  string_index idx{};
};
template <arg_of<node::variant> T>
void
construct(program& p, std::convertible_to<std::string_view> auto... args) {
  p.basic_blocks.back().nodes.emplace_back(p.idx.make<T>(args...));
}
template <arg_of<basic_block::jump_targets> T>
void
construct(program& p, std::convertible_to<std::string_view> auto... args) {
  p.basic_blocks.emplace_back(p.idx.make<T>(args...), std::vector<node>{});
};
inline std::string_view
add_string_to_index(program& p, std::string&& str) {
  return p.idx.add_string_to_index(std::move(str));
}
}  // namespace stanly

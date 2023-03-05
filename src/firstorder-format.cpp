#include "firstorder-lang.h"
#include <cstddef>
#include <fmt/format.h>
#include <functional>
#include <typeinfo>
#include <string_view>
#include <cassert>

#include <type_traits>

using stanly::metaprogramming::is_any_of_v;
using stanly::metaprogramming::type_name;
using stanly::metaprogramming::struct_to_tuple;
template<class T>
using is_firstorder_syntax_node = std::enable_if_t<is_any_of_v<T, stanly::FirstOderSyntaxNodes>, char>;

template<typename T>
struct fmt::formatter<T, is_firstorder_syntax_node<T>> : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const T& n, FormatContext& ctx) const {
    return formatter<string_view>::format("({} {})", type_name<T>, struct_to_tuple<T>(n),  ctx);
  }
};

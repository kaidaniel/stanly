#include "firstorder-lang.h"
#include <cstddef>
#include <fmt/format.h>
#include <functional>
#include <typeinfo>
#include <string_view>
#include <cassert>

#include <type_traits>


template<typename T>
struct fmt::formatter<T, std::enable_if_t<stanly::metaprogramming::is_any_of_v<T, stanly::FirstOderSyntaxNodes>, char>> : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const T& n, FormatContext& ctx) const {
    return formatter<string_view>::format("({} {})", stanly::metaprogramming::type_name<T>, stanly::metaprogramming::struct_to_tuple<T>(n),  ctx);
  }
};

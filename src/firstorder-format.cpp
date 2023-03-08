#include "firstorder-lang.h"
#include <cstdio>
#include <fmt/format.h>
#include <functional>
#include <string_view>

using stanly::metaprogramming::is_any_of_v;
using stanly::metaprogramming::struct_to_tuple;
using stanly::metaprogramming::type_name;
template <class T>
using is_firstorder_syntax_node =
    std::enable_if_t<is_any_of_v<T, stanly::FirstOrderSyntaxNode>, char>;

template <typename T>
struct fmt::formatter<T, is_firstorder_syntax_node<T>>
    : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const T &n, FormatContext &ctx) const {
    return formatter<string_view>::format(
        "({} {})", type_name<T>, struct_to_tuple<T>(n), ctx);
  }
};

namespace stanly {
template <class... T> void println(fmt::format_string<T...> fmt, T &&...args) {
  return fmt::print(stdout, "{}\n", fmt::format(fmt, std::forward<T>(args)...));
}

template <class... T> void print(fmt::format_string<T...> fmt, T &&...args) {
  return fmt::print(fmt, std::forward<T>(args)...);
}
} // namespace stanly
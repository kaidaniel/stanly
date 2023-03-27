#include <fmt/format.h>

#include <cstdio>
#include <functional>
#include <string_view>

#include "firstorder-lang.h"

using stanly::metaprogramming::contains;
using stanly::metaprogramming::struct_to_tpl;
using stanly::metaprogramming::type_name;
using std::string_view;
template <class T>
using is_firstorder_syntax_node =
    std::enable_if_t<contains<stanly::first_order<stanly::text_ref>, T>, char>;

template <typename T>
struct fmt::formatter<T, is_firstorder_syntax_node<T>> : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const T &n, FormatContext &ctx) const {
    return formatter<string_view>::format("({} {})", type_name<T>, struct_to_tpl<T>(n), ctx);
  }
};

namespace stanly {
template <class... T>
void println(fmt::format_string<T...> fmt, T &&...args) {
  return fmt::print(stdout, "{}\n", fmt::format(fmt, std::forward<T>(args)...));
}

template <class... T>
void print(fmt::format_string<T...> fmt, T &&...args) {
  return fmt::print(fmt, std::forward<T>(args)...);
}
}  // namespace stanly
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <cstdio>
#include <functional>
#include <string_view>

#include "firstorder-syntax.h"
#include "metaprogramming.h"

using stanly::metaprogramming::contains;
using stanly::metaprogramming::to_tpl;
using stanly::metaprogramming::type_name_suffix;
using std::string_view;
template <class T>
using is_firstorder_syntax_node =
    std::enable_if_t<contains<stanly::firstorder::syntax<string_view>::node, T>, char>;

template <typename T>
struct fmt::formatter<T, is_firstorder_syntax_node<T>> : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const T &n, FormatContext &ctx) const {
    return fmt::format_to(ctx.out(), "({} {})", type_name_suffix<T>, fmt::join(to_tpl(n), " "));
  }
};

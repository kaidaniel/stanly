#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <cstdio>
#include <functional>
#include <string_view>
#include <vector>

#include "firstorder-syntax.h"
#include "metaprogramming.h"

template <class T>
using is_firstorder_syntax_node = std::enable_if_t<
    stanly::metaprogramming::contains<stanly::firstorder::syntax<std::string_view>::node, T>, char>;

template <typename T>
struct fmt::formatter<T, is_firstorder_syntax_node<T>> : fmt::formatter<std::string_view> {
  template <class FormatContext>
  auto format(const T &n, FormatContext &ctx) const {
    return fmt::format_to(ctx.out(), "{}({})", stanly::metaprogramming::type_name_suffix<T>,
                          fmt::join(stanly::metaprogramming::to_tpl(n), " "));
  }
};

template <>
struct fmt::formatter<std::vector<stanly::firstorder::syntax<std::string_view>::node>>
    : fmt::formatter<std::string_view> {
  template <class FormatContext>
  auto format(const std::vector<stanly::firstorder::syntax<std::string_view>::node> &n,
              FormatContext &ctx) {
    constexpr int prefix{std::string_view{"variant("}.size()};
    std::vector<std::string> x{};
    std::transform(n.begin(), n.end(), std::back_inserter(x), [](const auto &variant) {
      auto s = fmt::format("{}", variant);
      return s.substr(prefix, s.size() - (prefix + 1));
    });
    return fmt::format_to(ctx.out(), "ast[{}]", fmt::join(x, " "));
  }
};

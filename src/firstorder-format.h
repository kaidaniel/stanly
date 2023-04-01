#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <cstdio>
#include <format>
#include <functional>
#include <numeric>
#include <ranges>
#include <string_view>
#include <variant>
#include <vector>

#include "firstorder-syntax.h"
#include "metaprogramming.h"

template <class T>
using is_firstorder_syntax_node = std::enable_if_t<
    stanly::metaprogramming::contains<stanly::firstorder::syntax<std::string_view>::node, T>, char>;

template <class T>
concept firstorder_syntax_node =
    stanly::metaprogramming::contains<stanly::firstorder::syntax<std::string_view>::node, T>;

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
template <class T>
struct std::formatter<std::vector<T>> : std::formatter<T> {
  template <class FormatContext>
  auto format(const std::vector<T> &vec, FormatContext &ctx) const {
    auto out = ctx.out();
    std::format_to(out, "{}", '[');
    for (const T &it = vec.begin(); it != vec.end(); ++it) {
      std::formatter<T>::format(*it, ctx);
      if (++it != vec.end()) {
        ctx << ", ";
      }
    }
    std::format_to(out, "{}", ']');
    return ctx;
  }
};
template <firstorder_syntax_node N, class CharT>
struct std::formatter<N, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const N &n, Ctx &ctx) const {
    using stanly::metaprogramming::to_tpl;
    using stanly::metaprogramming::type_name_suffix;
    // clang-format off
    auto to_ctx = [out = ctx.out()]<class T>(const T &x) {
      constexpr bool is_string_view = std::is_same_v<T, std::string_view>;
      if constexpr(is_string_view) { return std::format_to(out, "{}", x); }
      return std::format_to(out, "[err]");
    };
    auto to_ctx_with_delim = [&](const auto &x) {to_ctx(" "); return to_ctx(x);};
    auto join = [&](const auto &tpl_head, const auto &...tpl_tail) {
      to_ctx(tpl_head); return (to_ctx_with_delim(tpl_tail), ...);
    };
    // clang-format on
    std::formatter<std::string_view, CharT>::format(type_name_suffix<N>, ctx);
    return std::apply(join, to_tpl(n));
  }
};
template <>
struct std::formatter<stanly::firstorder::syntax<std::string_view>::node>
    : std::formatter<std::string_view> {
  template <class FormatContext>
  auto format(const stanly::firstorder::syntax<std::string_view>::node &node,
              FormatContext &ctx) const {
    std::format_to(ctx.out(), "'");  // mark as "marker to indicate type as variant<N>, not just N"
    return std::visit([&ctx]<class N>(const N &n) { return std::formatter<N>::format(n, ctx); },
                      node);
  }
};

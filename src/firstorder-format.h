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
  consteval auto parse(const std::format_parse_context &ctx) {
    element_format = std::basic_format_string<char>(
        std::string_view(std::begin(ctx), std::end(ctx))
      );
    return std::end(ctx);
  }
  template <class FormatContext>
  auto format(const std::vector<T> &vec, FormatContext &ctx) const {
    auto out = ctx.out();
    std::format_to(out, "{}", '[');
    for (auto it = vec.begin();; ++it) {
      std::format_to(out, element_format, std::make_format_args(*it));
      if (++it == vec.end()) { break; }
      std::format_to(out, "{}", ", ");
    }
    return std::format_to(out, "{}", ']');
  }

 private:
  std::basic_format_string<char> element_format;
};
template <firstorder_syntax_node N, class CharT>
struct std::formatter<N, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const N &n, Ctx &ctx) const {
    /*auto send = [out = ctx.out()](const auto &x) {
      if constexpr (requires { std::string{x}; }) {
        return std::format_to(out, "{}", x);
      }
      return std::format_to(out, "[err]");
    };*/
    auto send = [out = ctx.out()](const auto &x) { return std::format_to(out, "{}", x); };
    return std::apply(
        [&](const auto &tpl_head, const auto &...tpl_tail) {
          send(stanly::metaprogramming::type_name_suffix<N>);
          send("(");
          send(tpl_head);
          ((send(" "), send(tpl_tail)), ...);
          return send(")");
        },
        stanly::metaprogramming::to_tpl(n));
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

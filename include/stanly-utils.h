#pragma once

#include <__iterator/advance.h>

#include <cstdlib>
#include <format>
#include <iostream>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

#include "map"  // IWYU pragma: keep (used in template)

namespace stanly {
template <class x, class T>
constexpr bool arg_of_v = false;
template <class x, template <class...> class T, class... xs>
constexpr bool arg_of_v<x, T<xs...>> = std::disjunction_v<std::is_same<std::decay_t<x>, xs>...>;

template <class x, class T>
concept arg_of = arg_of_v<x, T>;

template <class T>
constexpr std::string_view type_name = []<class S = T> {
  std::string_view sv{__PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2};
  return sv.substr(sv.find_last_of(':') + 1);
}
();

// template<class T>
// constexpr std::string_view type_repr = type_name<T>;

[[noreturn]] inline void
unreachable(std::string_view msg = "") {
#ifndef NDEBUG
  std::cerr << std::format("Unreachable. {}\n", msg);
  std::exit(1);
#else
  [&]() { return msg; }();
  __builtin_unreachable();
#endif
}

}  // namespace stanly

template <class... Args, class CharT>
struct std::formatter<std::variant<Args...>, CharT> : std::formatter<std::string_view, CharT> {
  constexpr auto
  format(const std::variant<Args...> &v, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "inj-");
    return std::visit([&](auto &&x) { return std::format_to(ctx.out(), "{}", x); }, v);
  }
};

namespace stanly {
namespace {
constexpr auto
strlen(const char *str) {
  const char *end = str;
  while (*end != '\0') { ++end; }
  return end - str;
}
template <class Base = std::formatter<std::string_view>>
struct lines_arg_parser : Base {
  bool lines_arg = false;
  constexpr auto
  parse(auto &ctx) {
    constexpr const char *lines = "lines";
    constexpr auto len = strlen(lines);
    lines_arg =
        ((ctx.end() - ctx.begin()) >= len) && std::equal(ctx.begin(), ctx.begin() + len, lines);
    if (lines_arg) { return ctx.begin() + len; }
    return Base::parse(ctx);
  }
};
}  // namespace
}  // namespace stanly

template <template <class...> class Map, class Key, class Val, class... Args, class CharT>
  requires std::same_as<Map<Key, Val, Args...>, std::unordered_map<Key, Val, Args...>> ||
           std::same_as<Map<Key, Val, Args...>, std::map<Key, Val, Args...>>
struct std::formatter<Map<Key, Val, Args...>, CharT>
    : public stanly::lines_arg_parser<std::formatter<Val, CharT>> {
  auto
  format(const Map<Key, Val, Args...> &map, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "{");
    for (const auto &el : map) {
      const auto &[key, val] = el;
      if (this->lines_arg) {
        std::format_to(ctx.out(), "\n    {}: {},", key, val);
      } else {
        if (&el != &*(map.begin())) { std::format_to(ctx.out(), "{}", ", "); }
        std::format_to(ctx.out(), "{}: {}", key, val);
      }
    }
    if (this->lines_arg) { std::format_to(ctx.out(), "{}", "\n"); }
    return std::format_to(ctx.out(), "{}", "}");
  }
};

template <class El, class CharT>
struct std::formatter<std::vector<El>, CharT>
    : stanly::lines_arg_parser<std::formatter<El, CharT>> {
  constexpr auto
  format(const std::vector<El> vec, auto &ctx) const {
    std::format_to(ctx.out(), "{}", '[');
    for (const auto &el : vec) {
      if (this->lines_arg) {
        std::format_to(ctx.out(), "\n    {},", el);
      } else {
        if (&el != &*(vec.begin())) { std::format_to(ctx.out(), "{}", ", "); }
        std::formatter<El, CharT>{}.format(el, ctx);
      }
    }
    if (this->lines_arg) { std::format_to(ctx.out(), "{}", "\n"); }

    return std::format_to(ctx.out(), "{}", ']');
  }
};
template <class Arg, class... Args, class CharT>
struct std::formatter<std::tuple<Arg, Args...>, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const std::tuple<Arg, Args...> &tpl, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "(");
    std::apply(
        [&ctx](const auto &x, const auto &...xs) {
          std::format_to(ctx.out(), "{}", x), ((std::format_to(ctx.out(), " {}", xs)), ...);
        },
        tpl);
    return std::format_to(ctx.out(), "{}", ")");
  }
};
template <class CharT>
struct std::formatter<std::tuple<>, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const std::tuple<> &, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "()");
  }
};
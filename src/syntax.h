#pragma once

#include <concepts>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "stanly-utils.h"

namespace stanly {

template <class Repr>
struct lang {
  // clang-format off
  struct update { Repr tgt; Repr field; Repr src; };
  struct load   { Repr var; Repr src; Repr field; };
  struct lit    { Repr var; Repr value; };
  struct alloc  { Repr var; Repr type; };
  struct ref    { Repr var; Repr src; };
  // clang-format on
  using firstorder = std::variant<update, load, lit, alloc, ref>;
};
using nodes = lang<std::string_view>;

class idx {
 public:
  explicit operator size_t() const { return value; };
  using repr = uint16_t;
  explicit constexpr idx(size_t i) : value{static_cast<repr>(i)} {
    stanly_assert(i < (std::numeric_limits<idx::repr>::max() - 1),
                  std::format("{}-byte index can't support more than {} elements.",
                              sizeof(idx::repr), (std::numeric_limits<idx::repr>::max() - 2)));
  };
  constexpr idx() : value{0} {};
  constexpr bool operator<=>(const idx &other) const = default;

 private:
  repr value;
};
using packed_nodes = lang<idx>;
constexpr idx operator""_i(unsigned long long i) { return idx(i); }

template <class T>
concept syntax_node = contains<nodes::firstorder, std::decay_t<T>> ||
                      contains<packed_nodes::firstorder, std::decay_t<T>>;

template <class T>
concept syntax = std::same_as<nodes::firstorder, std::decay_t<T>> ||
                 std::same_as<packed_nodes::firstorder, std::decay_t<T>>;

const int kN_BYTES_PACKED = 8;
template <class T>
concept packed_syntax = syntax<T> && sizeof(std::declval<T>()) <= kN_BYTES_PACKED;

template <syntax_node X, syntax_node Y>
bool operator==(X &&x, Y &&y) {
  if constexpr (std::same_as<X, Y>) {
    return to_tpl(std::forward<X>(x)) == to_tpl(std::forward<Y>(y));
  }
  return false;
};
template <syntax S>
bool operator==(S &&s1, S &&s2) {
  return std::visit(std::equal_to{}, std::forward<S>(s1), std::forward<S>(s2));
}
template <class T>
  requires syntax_node<T> || syntax<T>
auto &operator<<(auto &s, const T &x) {
  return s << std::format("{}", x);
}
}  // namespace stanly

template <>
struct std::hash<stanly::idx> {
  auto operator()(const stanly::idx &idx) const {
    using cast_to = size_t;
    static_assert(std::unsigned_integral<cast_to>);
    static_assert(std::unsigned_integral<stanly::idx::repr>);
    static_assert(sizeof(cast_to) >= sizeof(stanly::idx::repr));
    return std::hash<cast_to>{}(static_cast<cast_to>(idx));
  }
};

template <class CharT>
struct std::formatter<stanly::idx, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::idx &idx, auto &ctx) const {
    return std::formatter<std::size_t, CharT>{}.format(static_cast<std::size_t>(idx), ctx);
  }
};

template <class El, class CharT>
struct std::formatter<std::vector<El>, CharT> : std::formatter<El, CharT> {
  constexpr auto format(const std::vector<El> vec, auto &ctx) const {
    std::format_to(ctx.out(), "{}", '[');
    for (const auto &el : vec) {
      if (&el != &*(vec.begin())) { std::format_to(ctx.out(), "{}", ", "); }
      std::formatter<El, CharT>::format(el, ctx);
    }
    return std::format_to(ctx.out(), "{}", ']');
  }
};
template <class... Args, class CharT>
struct std::formatter<std::tuple<Args...>, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const std::tuple<Args...> &tpl, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "(");
    if constexpr (!std::same_as<std::tuple<Args...>, std::tuple<>>) {
      std::apply(
          [&ctx](const auto &x, const auto &...xs) {
            std::format_to(ctx.out(), "{}", x), ((std::format_to(ctx.out(), " {}", xs)), ...);
          },
          tpl);
    }
    return std::format_to(ctx.out(), "{}", ")");
  }
};
template <stanly::syntax_node T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const T x, auto &ctx) const {
    std::formatter<std::string_view, CharT>::format(stanly::type_name<T>, ctx);
    using tpl_type = std::decay_t<decltype(to_tpl(x))>;
    return static_cast<std::formatter<tpl_type>>(*this).format(to_tpl(x), ctx);
  }
};

template <class... Args, class CharT>
struct std::formatter<std::variant<Args...>, CharT> : std::formatter<std::string_view, CharT> {
  constexpr auto format(const std::variant<Args...> &v, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "inj-");
    return std::visit([&](auto &&x) { return std::format_to(ctx.out(), "{}", x); }, v);
  }
};

template <class Key, class Val, class... Args, class CharT>
struct std::formatter<std::unordered_map<Key, Val, Args...>, CharT> : std::formatter<Val, CharT> {
  auto format(const std::unordered_map<Key, Val, Args...> &map, auto &ctx) const {
    std::format_to(ctx.out(), "{}", "{");
    for (const auto &el : map) {
      auto &[key, val] = el;
      if (&el != &*(map.begin())) { std::format_to(ctx.out(), "{}", ", "); }
      std::format_to(ctx.out(), "{}", key);
      std::format_to(ctx.out(), "{}", ": ");
      std::format_to(ctx.out(), "{}", val);
    }
    return std::format_to(ctx.out(), "{}", "}");
  }
};
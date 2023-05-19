#pragma once

#include <concepts>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "stanly-utils.h"

namespace stanly {

class idx {
 public:
  explicit operator size_t() const { return value; };
  using repr = uint16_t;
  explicit idx(size_t i) : value{static_cast<repr>(i)} {
    stanly_assert(i < (std::numeric_limits<idx::repr>::max() - 1),
                  std::format("{}-byte index can't support more than {} elements.",
                              sizeof(idx::repr), (std::numeric_limits<idx::repr>::max() - 2)));
  };
  idx() : value{0} {};
  bool operator<=>(const idx& other) const = default;

 private:
  repr value;
};

template <class T>
struct is_syntax_node {
  constexpr static bool value = false;
};

template <class T>
concept syntax_node = is_syntax_node<T>::value;

template <class T>
concept syntax = all<is_syntax_node, std::decay_t<T>> &&
                 requires(T t) { std::visit([](auto&&) { return 1; }, t); };

template <class T>
struct is_syntax {
  constexpr static bool value = syntax<T>;
};

const int kN_BYTES_PACKED = 8;
template <class T>
concept packed_syntax = syntax<T> && sizeof(std::declval<T>()) <= kN_BYTES_PACKED;

template <syntax_node X, syntax_node Y>
bool operator==(X&& x, Y&& y) {
  if constexpr (std::same_as<X, Y>) {
    return to_tpl(std::forward<X>(x)) == to_tpl(std::forward<Y>(y));
  }
  return false;
};
template <stanly::syntax S>
bool operator==(S&& s1, S&& s2) {
  return std::visit(std::equal_to{}, std::forward<S>(s1), std::forward<S>(s2));
}
}  // namespace stanly

template <>
struct std::hash<stanly::idx> {
  auto operator()(const stanly::idx& idx) const {
    return std::hash<size_t>{}(static_cast<size_t>(idx));
  }
};

template <class CharT>
struct std::formatter<stanly::idx, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const stanly::idx& idx, Ctx& ctx) const {
    return std::formatter<std::size_t, CharT>{}.format(static_cast<std::size_t>(idx), ctx);
  }
};
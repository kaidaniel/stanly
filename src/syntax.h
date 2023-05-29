#pragma once

#include <concepts>
#include <format>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "handle.h"
#include "stanly-utils.h"

namespace stanly {

template <class Repr>
struct lang {
  // clang-format off
  struct alloc  { Repr var; Repr type; };
  struct lit    { Repr var; Repr type; Repr value; };
  struct ref    { Repr var; Repr src; };
  struct update { Repr tgt; Repr field; Repr src; };
  struct load   { Repr var; Repr src; Repr field; };
  // clang-format on
  using firstorder = std::variant<update, load, lit, alloc, ref>;
};
using nodes = lang<std::string_view>;

using packed_nodes = lang<handle>;

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

template <stanly::syntax_node T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const T x, auto &ctx) const {
    std::formatter<std::string_view, CharT>::format(stanly::type_name<T>, ctx);
    using tpl_type = std::decay_t<decltype(to_tpl(x))>;
    return static_cast<std::formatter<tpl_type>>(*this).format(to_tpl(x), ctx);
  }
};
#pragma once

#include <concepts>
#include <string_view>
#include <variant>
#include <vector>

namespace stanly {

using idx = uint16_t;

template <class S>
concept syntax = requires(typename S::node s) {
  typename S::repr;
  typename S::node;
  { std::variant{s} } -> std::same_as<typename S::node>;
};

template <class T, class x>
constexpr bool contains = false;

template <class x, class... xs>
constexpr bool contains<std::variant<xs...>, x> = std::disjunction_v<std::is_same<x, xs>...>;

template <syntax T, class x>
constexpr bool contains<T, x> = contains<typename T::node, x>;

}  // namespace stanly

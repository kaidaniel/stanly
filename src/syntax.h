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

}  // namespace stanly

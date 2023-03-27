#pragma once

#include <concepts>
#include <string_view>
#include <variant>

#include "iterator.h"

namespace stanly {

using idx = uint16_t;

template <class S>
concept syntax = requires(typename S::node s) {
  typename S::repr;
  typename S::node;
  { std::variant{s} } -> std::same_as<typename S::node>;
};

template <syntax S>
  requires std::same_as<typename S::repr, std::string_view>
iterator::inpt_range<typename S::node> parse(typename S::repr);
}  // namespace stanly

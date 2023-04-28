#pragma once

#include <concepts>
#include <string_view>
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
concept syntax =
    all<is_syntax_node, T> && requires(T t) { std::visit([](auto&&) { return 1; }, t); };

template <class T>
concept packed_syntax = syntax<T> && sizeof(std::declval<T>()) <= 8;

template <packed_syntax T>
struct associated_unpacked_syntax;

template <packed_syntax T>
using associated_unpacked_syntax_t = typename associated_unpacked_syntax<T>::type;

// template<template<class>class T, class x> requires packed_syntax<T<x>>
// struct resolve_idx { using type = T<};

}  // namespace stanly

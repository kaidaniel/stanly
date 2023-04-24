#pragma once

#include <concepts>
#include <string_view>
#include <variant>
#include <vector>

#include "stanly-utils.h"

namespace stanly {

class idx {
  uint16_t value;

 public:
  explicit operator size_t() const { return value; };
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

// template<template<class>class T, class x> requires packed_syntax<T<x>>
// struct resolve_idx { using type = T<};

}  // namespace stanly

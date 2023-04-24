#pragma once

#include <concepts>
#include <string_view>
#include <variant>
#include <vector>

#include "stanly-utils.h"

namespace stanly {

using idx = uint16_t;

template <class T>
struct is_syntax_node {
  constexpr static bool value = false;
};

template <class T>
concept syntax =
    all<is_syntax_node, T> && requires(T t) { std::visit([](auto&&) { return 1; }, t); };

}  // namespace stanly

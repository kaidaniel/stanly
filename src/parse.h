#pragma once
#include "syntax.h"

template <syntax S>
  requires std::same_as<typename S::repr, std::string_view>
std::vector<typename S::node> parse(typename S::repr) {}

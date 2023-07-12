#pragma once

#include <format>
#include <iostream>
#include <source_location>
#include <string_view>

#ifdef NDEBUG
#define stanly_assert(...)
#else
inline void
stanly_assert(bool condition, std::string_view msg = "",
    std::source_location sl = std::source_location::current()) {
  if (!condition) {
    constexpr std::string_view fmt = "{}:{}: {}: Assertion failed. {}\n";
    std::cerr << std::format(fmt, sl.file_name(), sl.line(), sl.function_name(), msg);
    std::abort();
  }
}
#endif
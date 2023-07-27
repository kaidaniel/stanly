#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <limits>
#include <ostream>
#include <string_view>

#include "stanly-assert.h"

namespace stanly {
class handle {
 public:
  explicit operator size_t() const { return value; };
  using repr = uint16_t;
  explicit constexpr handle(size_t i) : value{static_cast<repr>(i)} {
    stanly_assert(
        i < (std::numeric_limits<handle::repr>::max() - 1),
        std::format(
            "{}-byte index can't support more than {} elements.", sizeof(handle::repr),
            (std::numeric_limits<handle::repr>::max() - 2)));
  };
  constexpr handle() : value{0} {};
  constexpr std::strong_ordering operator<=>(const handle& other) const = default;
  friend constexpr std::ostream& operator<<(std::ostream& os, const stanly::handle& h);

 private:
  repr value;
};

constexpr std::ostream&
operator<<(std::ostream& os, const stanly::handle& h) {
  return os << h.value;
}
constexpr handle
operator""_i(unsigned long long i) {
  return handle(i);
}

}  // namespace stanly

template <>
struct std::hash<stanly::handle> {
  auto
  operator()(const stanly::handle& handle) const {
    using cast_to = size_t;
    static_assert(std::unsigned_integral<cast_to>);
    static_assert(std::unsigned_integral<stanly::handle::repr>);
    static_assert(sizeof(cast_to) >= sizeof(stanly::handle::repr));
    return std::hash<cast_to>{}(static_cast<cast_to>(handle));
  }
};

template <class CharT>
struct std::formatter<stanly::handle, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const stanly::handle& handle, auto& ctx) const {
    return std::formatter<std::size_t, CharT>{}.format(static_cast<std::size_t>(handle), ctx);
  }
};
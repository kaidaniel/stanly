#pragma once

#include <format>
#include <iostream>
#include <source_location>
#include <string_view>

#ifdef NDEBUG
#define stanly_assert(...)
#else
inline void stanly_assert(bool condition, std::string_view msg = "",
                          std::source_location sl = std::source_location::current()) {
  if (!condition) {
    constexpr std::string_view fmt = "{}:{}: {}: Assertion failed. {}\n";
    std::cerr << std::format(fmt, sl.file_name(), sl.line(), sl.function_name(), msg);
    std::abort();
  }
}
#endif

namespace stanly {
template <class T, class x>
constexpr bool contains = false;
template <class x, class... xs>
constexpr bool contains<std::variant<xs...>, x> = std::disjunction_v<std::is_same<x, xs>...>;

template <template <class> class Predicate, class Variant>
struct holds_for_all_types_of;

template <template <class> class Predicate, class... xs>
struct holds_for_all_types_of<Predicate, std::variant<xs...>> {
  constexpr static bool value = (Predicate<xs>::value && ...);
};

template <template <class> class Predicate, class Variant>
concept all = holds_for_all_types_of<Predicate, Variant>::value;

template <class T>
constexpr std::string_view type_name = []<class S = T> {
  std::string_view sv{__PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2};
  return sv.substr(sv.find_last_of(':') + 1);
}
();

template <class T, class Variant>
struct search_same_name;

template <class T, class Variant>
using search_same_name_t = typename search_same_name<T, Variant>::type;

template <class T, class x, class... xs>
struct search_same_name<T, std::variant<x, xs...>> {
  using type =
      std::conditional<type_name<x> == type_name<T>, x, search_same_name_t<T, std::variant<xs...>>>;
};

auto to_tpl(auto &&object) noexcept {
  using type = std::decay_t<decltype(object)>;
  if constexpr (requires(type t) { type{{}, {}, {}, {}, {}}; }) {
    auto &&[p1, p2, p3, p4, p5] = object;
    return std::make_tuple(p1, p2, p3, p4, p5);
  } else if constexpr (requires(type t) { type{{}, {}, {}, {}}; }) {
    auto &&[p1, p2, p3, p4] = object;
    return std::make_tuple(p1, p2, p3, p4);
  } else if constexpr (requires(type t) { type{{}, {}, {}}; }) {
    auto &&[p1, p2, p3] = object;
    return std::make_tuple(p1, p2, p3);
  } else if constexpr (requires(type t) { type{{}, {}}; }) {
    auto &&[p1, p2] = object;
    return std::make_tuple(p1, p2);
  } else if constexpr (requires(type t) { type{{}}; }) {
    auto &&[p1] = object;
    return std::make_tuple(p1);
  } else {
    return std::make_tuple();
  }
}

[[noreturn]] inline void unreachable(std::string_view msg = "") {
#ifndef NDEBUG
  std::cerr << std::format("Unreachable. {}\n", msg);
  std::abort();
#else
  __builtin_unreachable()
#endif
}

}  // namespace stanly

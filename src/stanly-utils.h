#pragma once

#include <format>
#include <iostream>
#include <ranges>
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
    static_assert(std::default_initializable<type>);
    return std::make_tuple();
  }
}

template <template <class...> class T, class... xs>
auto map_members(auto &&f) {
  return [&]<class X>(X &&x) -> T<X, xs...> {
    return std::apply(
        [&]<class... El>(El &&...el) { return T<X, xs...>{f(std::forward<El>(el))...}; },
        to_tpl(std::forward<X>(x)));
  };
}

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
struct search_same_name {
  using type = std::false_type;
};

template <class T, class x, class... xs>
struct search_same_name<T, std::variant<x, xs...>> {
  using type =
      std::conditional_t<type_name<std::remove_cvref_t<x>> == type_name<std::remove_cvref_t<T>>, x,
                         typename search_same_name<T, std::variant<xs...>>::type>;
};

template <class T, class Variant>
using search_same_name_t = typename search_same_name<T, Variant>::type;

template <class x, template <class...> class T>
constexpr static bool is_instance_of = false;
template <class... xs, template <class...> class T>
constexpr static bool is_instance_of<T<xs...>, T> = true;
template <class x, template <class...> class T>
concept instance_of = is_instance_of<x, T>;

template <class Variant1, class Variant2>
concept variants_with_same_type_names =
    instance_of<Variant1, std::variant> && instance_of<Variant2, std::variant> &&
    std::variant_size_v<Variant1> == std::variant_size_v<Variant2> &&
    []<std::size_t... Is>(std::index_sequence<Is...>) {
      return ((type_name<std::variant_alternative_t<Is, Variant1>> ==
               type_name<std::variant_alternative_t<Is, Variant2>>)&&...);
    }(std::make_index_sequence<std::variant_size_v<Variant1>>{});

template <class Variant1, class Variant2>
concept variants_with_same_tuple_sizes =
    instance_of<Variant1, std::variant> && instance_of<Variant2, std::variant> &&
    std::variant_size_v<Variant1> == std::variant_size_v<Variant2> &&
    []<std::size_t... Is>(std::index_sequence<Is...>) {
      return ((std::tuple_size_v<decltype(to_tpl(
                   std::declval<std::variant_alternative_t<Is, Variant1>>()))> ==
               std::tuple_size_v<decltype(to_tpl(
                   std::declval<std::variant_alternative_t<Is, Variant2>>()))>)&&...);
    }(std::make_index_sequence<std::variant_size_v<Variant1>>{});

// exists [a], [b]. A: variant<a...>, B: variant<b...>, len(A)==len(B), len(a)==len(b)&&...,
// type_name<a>==type_name<b>&&..., map_to_same_name: (intersect(a_i->b_i)...) -> A -> B
template <class A, class B, class F>
  requires variants_with_same_type_names<A, B> && variants_with_same_tuple_sizes<A, B>
constexpr auto map_to_same_name(F &&member_a_to_member_b) {
  auto inja_to_injb = map_members<search_same_name_t, B>(member_a_to_member_b);
  auto inja_to_b = [=](auto &&inja) { return B{inja_to_injb(inja)}; };
  return [=](auto &&a) { return std::visit(inja_to_b, a); };
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

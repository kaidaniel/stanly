#pragma once
#include <boost/core/demangle.hpp>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>

namespace stanly::metaprogramming {

template <class T>
const std::string type_name = [] {
  const boost::core::scoped_demangled_name type_name_of_T{typeid(T).name()};
  const std::string_view v{type_name_of_T.get()};
  std::string ret;
  using size = std::string_view::size_type;
  size left = 0;
  size right = 0;
  size offset = 0;

  while (left < v.size()) {
    right = v.find_first_of("<>,", left);
    right = v.find_first_not_of("<>,", right);
    right = right == std::string_view::npos ? v.size() : right;
    offset = v.rfind("::", right);
    offset = offset == std::string_view::npos ? left : (offset + 2);
    offset = offset > left ? offset : left;
    ret += v.substr(offset, right - offset);
    left = right + 1;
  }
  return ret;
}();

auto struct_to_tpl(auto &&object) noexcept {
  using type = std::decay_t<decltype(object)>;
  if constexpr (requires(type t) { type{{}, {}, {}, {}}; }) {
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

template <class... T> using lookup = void;

template <int Idx, template <class> class TL> struct get_s;

template <int Idx, template <class> class TL>
using get = typename get_s<Idx, TL>::type;

template <class T, template <class> class TL> struct idx_of_s;

template <class T, template <class> class TL>
static constexpr int idx_of = idx_of_s<T, TL>::value;

template <
    class T, template <class> class From, template <class> class To, class F>
get<idx_of<T, From>, To> transmute(F &&f, get<idx_of<T, From>, From> &&from) {
  return {f(struct_to_tpl(std::forward<From>(from)))};
}

// TypeList: {T: [*] -> *} (xs:[*]) ->
// record {
//  empty: <(e:[*]), T e>
//  push: (x: *) T xs -> T x::xs
//  head: {x: *} T x::xs -> x
//  tail: {x: *} T x::xs -> T xs
// }
// concept: is_empty, head, tail, push
template<class TypeList> static constexpr bool is_empty = false;
static constexpr bool empty = true;
static constexpr bool not_empty = false;
template<class L> struct head_s;
template<class T> struct tail_s;
template<class A, class T> struct push_s;
template<class T> using head = typename head_s<T>::type;
template<class T> using tail = typename tail_s<T>::type;
template<class A, class T> using push = typename push_s<A, T>::type;
template<class T> concept TypeList = 
  requires {typename push<struct any_type, T>; } && 
  (requires { typename head<T>; typename tail<T>; } || is_empty<T>);

// TypeList (typelist: [*] -> *)
template <class... A> struct typelist;
template<> static constexpr bool is_empty<typelist<>> = true;
template<class Head, class ...Tail>
struct head_s<typelist<Head, Tail...>> { using type = Head; };
template<class Head, class ...Tail>
struct tail_s<typelist<Head, Tail...>> { using type = typelist<Tail...>;};
template<class A, class ...Args>
struct push_s<A, typelist<Args...>> { using type = typelist<A, Args...>;};
static_assert(TypeList<typelist<>>);
static_assert(TypeList<typelist<typelist<int, bool>, typelist<>>>);
static_assert(std::is_same_v<push<int, push<bool, push<float, typelist<>>>>,/*==*/typelist<int, bool, float>>);
static_assert(std::is_same_v<head<typelist<bool, float, int>>,/*==*/bool>);
static_assert(std::is_same_v<tail<typelist<int, bool, float>>,/*==*/typelist<bool, float>>);
static_assert(std::is_same_v<tail<tail<typelist<int, bool, float>>>,/*==*/typelist<float>>);
static_assert(std::is_same_v<head<tail<tail<typelist<int, bool, float>>>>,/*==*/float>);

// idx: int -> TypeList -> *
template<unsigned n, TypeList T> struct idx_s       : idx_s<n-1, tail<T>>{};
template<TypeList T>             struct idx_s<0, T> : head_s<T>{};
template<unsigned n, TypeList T>  using idx         = typename idx_s<n, T>::type;
static_assert(std::is_same_v<idx<0, typelist<int, bool, float>>, int>);
static_assert(std::is_same_v<idx<1, typelist<int, bool, float>>, bool>);
static_assert(std::is_same_v<idx<2, typelist<int, bool, float>>, float>);
static_assert(idx<0, typelist<std::integral_constant<int, 0>, std::integral_constant<int, 1>>>::value == 0);

// contains: {T: [*] -> *}{xs: [*]} * -> T xs -> bool
template <class x, class T>
constexpr static bool contains = false;
template <class x, class... xs, template<class...> class T> 
constexpr static bool contains<T<xs...>, x> = std::disjunction_v<std::is_same<x, xs>...>;
static_assert(not contains<int, int>);
static_assert(contains<typelist<int>, int>);
static_assert(not contains<typelist<int>, typelist<int>>);
static_assert(contains<typelist<typelist<>>, typelist<>>);

// rebind: {T: [*] -> *}{xs: [*]}(* -> *) -> T xs -> *
template<template<class...> class T, class T_xs> struct rebind_s;
template<template<class...> class T, template<class...> class T2, class... x>
struct rebind_s<T, T2<x...>> { using type = T<x...>; };
template<template<class...> class T, class T_xs>
using rebind = typename rebind_s<T, T_xs>::type;
static_assert(std::is_same_v<rebind<typelist, typelist<int, double, bool>>, typelist<int, double, bool>>);

// append: {xs: [*]} (x: *) -> TypeList xs -> TypeList xs::x
template<class x, TypeList T, bool = is_empty<T>> struct append_s;
template<class x, TypeList T> using append = typename append_s<x, T>::type;
template<class x, TypeList T> struct append_s<x, T, empty>     : push_s<x,T>{};
template<class x, TypeList T> struct append_s<x, T, not_empty> : push_s<head<T>, append<x, tail<T>>> {};
// optimisation for typelist: O(1) instantiations (vs. O(n))
template<class x, class... xs> struct append_s<x, typelist<xs...>, not_empty> { using type = typelist<xs..., x>; };
static_assert(std::is_same_v<append<int, typelist<>>, 
                             typelist<int>>);
static_assert(std::is_same_v<append<typelist<int, int>, typelist<int>>, 
                             typelist<int, typelist<int, int>>>);
static_assert(std::is_same_v<append<bool, typelist<int, char, double>>,
                             typelist<int, char, double, bool>>);

// map: (f: * -> *) -> TypeList -> TypeList
template<template<class> class f, TypeList T, bool = is_empty<T>> struct map_s;
template<template<class> class f, TypeList T, bool = is_empty<T>> using map = typename map_s<f, T>::type;
template<template<class> class f, TypeList T> struct map_s<f, T,     empty> { using type = T;};
template<template<class> class f, TypeList T> struct map_s<f, T, not_empty> : push_s<f<head<T>>, map<f, tail<T>>>{};
static_assert(std::is_same_v<map<std::add_pointer_t, typelist<>>, typelist<>>);
static_assert(std::is_same_v<map<std::add_pointer_t, typelist<int>>, typelist<int*>>);
static_assert(std::is_same_v<map<std::add_pointer_t, typelist<int, bool, char>>, typelist<int*, bool*, char*>>);
static_assert(std::is_same_v<map<std::decay_t, typelist<int&&, const int&&, const int&, int*&>>, typelist<int, int, int, int*>>);

// reduce: (f: * -> * -> *) -> (i: *) -> (TypeList: L) -> *
template<template<class, class> class f, class i, TypeList T, bool = is_empty<T>> struct reduce_s;
template<template<class, class> class f, class i, TypeList T, bool = is_empty<T>> using reduce = typename reduce_s<f, i, T>::type;
template<template<class, class> class f, class i, TypeList T> struct reduce_s<f, i, T, empty> { using type = i;};
template<template<class, class> class f, class x, TypeList T> struct reduce_s<f, x, T, not_empty> : reduce_s<f, f<head<T>, x>, tail<T>>{};


// largest: {T: TypeList}{x: [*]} T x -> *
template<class x, class y> using choose_larger = std::conditional_t<(sizeof(x) >= sizeof(y)), x, y>;
template<TypeList T> using largest = reduce<choose_larger, head<T>, tail<T>>;
static_assert(std::is_same_v<largest<typelist<short, bool, char, double>>, double>);
static_assert(std::is_same_v<
  largest<typelist<short, bool, std::tuple<int, int, int>, double>>,/*==*/std::tuple<int, int, int>>);

// reverse: TypeList -> typelist
template<TypeList T> using reverse = reduce<push, typelist<>, T>;
static_assert(std::is_same_v<reverse<typelist<int, bool, double, char>>, typelist<char, double, bool, int>>);


} // namespace stanly::metaprogramming

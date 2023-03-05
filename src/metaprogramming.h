#pragma once
#include <type_traits>
#include <string_view>
#include <tuple>
#include <string>
#include <typeinfo>
#include <cassert>
#include <boost/core/demangle.hpp>

namespace stanly::metaprogramming {

    template<class T>
const std::string type_name = []{
    constexpr std::string_view prefix = "stanly::";
    const boost::core::scoped_demangled_name type_name_of_T{typeid(T).name()};
    const std::string_view v{type_name_of_T.get()};

    assert((std::string_view{begin(v), begin(v) + prefix.size()}) == prefix);
    return std::string{begin(v) + prefix.size(), end(v)};
}();




template<class T>
auto struct_to_tuple(T&& object) noexcept {
    using type = std::decay_t<T>;
    if constexpr(requires(type t) { type{{}, {}, {}}; }) {
      auto&& [p1, p2, p3] = object;
      return std::make_tuple(p1, p2, p3);
    } else if constexpr(requires(type t) { type{{}, {}}; }) {
      auto&& [p1, p2] = object;
      return std::make_tuple(p1, p2);
    } else {
        return std::make_tuple();
    }
}
    template<class Head, class ...Tails>
    struct TypeList;

    template<template<class...> typename T, class ... Args>
    struct rebind;


    template<template<class...> typename T, class ... Args>
    struct rebind<T, TypeList<Args...>> {
    using type = T<Args...>;
    };


    template<class T, class... Args>
    struct is_any_of{
    constexpr static bool value = false;
    };

    template<class T, class ...Args>
    struct is_any_of<T, TypeList<Args...>>{
    constexpr static bool value = std::disjunction_v<std::is_same<T, Args>...>;
    };

    template<class T, class S>
    constexpr static bool is_any_of_v = is_any_of<T, S>::value;
}
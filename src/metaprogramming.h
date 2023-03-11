#pragma once
#include <boost/core/demangle.hpp>
#include <cassert>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <typeinfo>

namespace stanly::metaprogramming {

template <class T>
const std::string type_name = [] {
  constexpr std::string_view prefix = "stanly::";
  const boost::core::scoped_demangled_name type_name_of_T{typeid(T).name()};
  const std::string_view v{type_name_of_T.get()};

  assert((std::string_view{begin(v), begin(v) + prefix.size()}) == prefix);
  return std::string{begin(v) + prefix.size(), end(v)};
}();

template <class T> auto struct_to_tpl(T &&object) noexcept {
  using type = std::decay_t<T>;
  if constexpr (requires(type t) { type{{}, {}, {}}; }) {
    auto &&[p1, p2, p3] = object;
    return std::make_tuple(p1, p2, p3);
  } else if constexpr (requires(type t) { type{{}, {}}; }) {
    auto &&[p1, p2] = object;
    return std::make_tuple(p1, p2);
  } else {
    return std::make_tuple();
  }
}
template <class Head, class... Tails> struct TypeList;
template <template <class...> typename T, class... Args> struct rebind;
template <template <class...> typename T, class... Args>
struct rebind<T, TypeList<Args...>> {
  using type = T<Args...>;
};
template <template <class...> typename T, class... Args>
using rebind_t = typename rebind<T, Args...>::type;

template <class T, class... Args> constexpr static bool is_any_of_v = false;
template <class T, class... Args>
constexpr static bool is_any_of_v<T, TypeList<Args...>> =
    std::disjunction_v<std::is_same<T, Args>...>;

template<class ...T>
using lookup = void;

template<class T, class ...Args>
T tpl_to_struct(Args&&... args){ return T{std::forward<Args>(args)...}; }

template<int Idx, template<class> class TL>
struct get_s;

template<int Idx, template<class> class TL>
using get = typename get_s<Idx, TL>::type;

template<class T, template<class> class TL>
struct idx_of_s;

template<class T, template<class> class TL>
static constexpr int idx_of = idx_of_s<T, TL>::value;


template<class T, template<class> class From, template<class> class To, class F>
get<idx_of<T, From>, To> transmute(F&& f, get<idx_of<T, From>, From>&& from) {
  return {f(struct_to_tpl(std::forward<From>(from)))}; }


} // namespace stanly::metaprogramming

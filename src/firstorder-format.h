#include <cstdio>
#include <format>
#include <functional>
#include <numeric>
#include <ranges>
#include <string_view>
#include <variant>
#include <vector>

#include "firstorder-syntax.h"

namespace stanly {

template <class T>
constexpr std::string_view type_name = std::invoke([]<class S = T> {
  std::string_view sv{__PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2};
  return sv.substr(sv.find_last_of(':') + 1);
});

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

template <class CharT, class Ctx>
struct format {
  Ctx &ctx_;
  template <class T>
  decltype(auto) join(const std::vector<T> &vec) {
    for (const auto &el : vec) {
      (*this)(el);
      if (&el != &*(vec.end() - 1)) { (*this)(", "); }
    }
    return ctx_.out();
  }
  template <class... Ts>
  decltype(auto) join(const std::tuple<Ts...> &tpl) {
    auto sep = [this](const auto &head, const auto &...tail) {
      return (*this)(head), (((*this)(" "), (*this)(tail)), ...);
    };
    return std::apply(sep, tpl);
  }
  template <class T>
  decltype(auto) operator()(const T &t) {
    using type = std::conditional_t<std::same_as<std::decay_t<T>, char *>, std::string_view, T>;
    const type &tt = t;
    return std::formatter<type, CharT>{}.format(tt, ctx_);
  }
  decltype(auto) operator()(const auto &t, const auto &...ts) {
    return (*this)(t), ((*this)(ts), ...);
  }
  template <class T>
  decltype(auto) operator()(const std::vector<T> &v) {
    return (*this)("["), join(v), (*this)("]");
  }
  template <class... Ts>
  decltype(auto) operator()(const std::tuple<Ts...> &t) {
    return (*this)("("), join(t), (*this)(")");
  }
  template <class... Ts>
  decltype(auto) operator()(const std::variant<Ts...> &v) {
    // variants are like Σ-types, introduced by inj.
    return (*this)("inj-"), std::visit(*this, v);
  }
};
template <class Ctx>
format(Ctx &ctx) -> format<typename Ctx::char_type, Ctx>;
}  // namespace stanly

template <class T, class CharT>
struct std::formatter<std::vector<T>, CharT> : std::formatter<T, CharT> {
  template <class Ctx>
  auto format(const std::vector<T> &vec, Ctx &ctx) const {
    return stanly::format{ctx}(vec);
  }
};
template <class T>
concept syntax_node = stanly::contains<stanly::firstorder::syntax<std::string_view>, T>;

template <syntax_node SyntaxNode, class CharT>
struct std::formatter<SyntaxNode, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const SyntaxNode &syntax_node, Ctx &ctx) const {
    return stanly::format{ctx}(stanly::type_name<SyntaxNode>, stanly::to_tpl(syntax_node));
  }
};
template <syntax_node... SyntaxNodes, class CharT>
struct std::formatter<std::variant<SyntaxNodes...>, CharT> : std::formatter<std::string_view> {
  template <class Ctx>
  auto format(const std::variant<SyntaxNodes...> &variant, Ctx &ctx) const {
    return stanly::format{ctx}(variant);
  }
};

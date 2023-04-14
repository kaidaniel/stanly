#pragma once

#include <format>
#include <string_view>
#include <tuple>
#include <variant>
#include <vector>

#include "firstorder-syntax.h"

namespace stanly {

template <class T>
constexpr std::string_view type_name = []<class S = T> {
  std::string_view sv{__PRETTY_FUNCTION__, sizeof(__PRETTY_FUNCTION__) - 2};
  return sv.substr(sv.find_last_of(':') + 1);
}
();

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

template <class x, template <class...> class T>
constexpr static bool is_instance_of = false;
template <class... xs, template <class...> class T>
constexpr static bool is_instance_of<T<xs...>, T> = true;
template <class x, template <class...> class T>

concept instance_of = is_instance_of<x, T>;
template <class T>
concept syntax_node = contains<firstorder::syntax<std::string_view>, T>;

template <class T>
concept formatted_type = instance_of<T, std::vector> || instance_of<T, std::tuple> ||
                         instance_of<T, std::variant> || syntax_node<T>;

template <class CharT, class Ctx>
struct format {
  Ctx *ctx_;
  template <class T>
  decltype(ctx_->out()) fmt(const T &t) {
    if constexpr (instance_of<T, std::vector>) {
      fmt("[");
      for (const auto &el : t) {
        if (&el != &*(t.begin())) { fmt(", "); }
        fmt(el);
      }
      fmt("]");
    } else if constexpr (instance_of<T, std::variant>) {
      fmt("inj-");
      std::visit(*this, t);
    } else if constexpr (instance_of<T, std::tuple>) {
      fmt("(");
      std::apply([this](const auto &x, const auto &...xs) { fmt(x), ((fmt(" "), fmt(xs)), ...); },
                 t);
      fmt(")");
    } else if constexpr (std::same_as<std::decay_t<T>, char *>) {
      std::formatter<std::string_view, CharT>{}.format(std::string_view{t}, *ctx_);
    } else if constexpr (syntax_node<T>) {
      fmt(type_name<T>);
      fmt(to_tpl(t));
    } else {
      std::formatter<T, CharT>{}.format(t, *ctx_);
    }
    return ctx_->out();
  }
  decltype(auto) operator()(const auto &t) { return fmt(t); }
};
template <class Ctx>
format(Ctx *ctx) -> format<typename Ctx::char_type, Ctx>;
}  // namespace stanly

template <stanly::formatted_type T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const T &t, Ctx &ctx) const {
    return stanly::format{&ctx}(t);
  }
};
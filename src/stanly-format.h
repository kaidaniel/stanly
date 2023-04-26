#pragma once

#include <format>
#include <string_view>
#include <tuple>
#include <variant>
#include <vector>

#include "stanly-utils.h"
#include "syntax.h"

namespace stanly {

template <class T>
concept formatted_type = instance_of<T, std::vector> || instance_of<T, std::tuple> ||
                         instance_of<T, std::variant> || syntax_node<T>;

template <class CharT, class Ctx>
struct format {
  Ctx *ctx_;
  template <class T>
  void fmt(const T &t) {
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
  }
  decltype(auto) operator()(const auto &t) {
    fmt(t);
    return ctx_->out();
  }
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
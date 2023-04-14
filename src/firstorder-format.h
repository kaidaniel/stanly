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

template <class x, template <class...> class T>
constexpr static bool is_instance_of = false;
template <class... xs, template <class...> class T>
constexpr static bool is_instance_of<T<xs...>, T> = true;
template <class T>
concept tuple_instance = is_instance_of<T, std::tuple>;
template <class T>
concept vector_instance = is_instance_of<T, std::vector>;
template <class T>
concept variant_instance = is_instance_of<T, std::variant>;
template <class T>
concept syntax_node = stanly::contains<stanly::firstorder::syntax<std::string_view>, T>;

template <class CharT, class Ctx>
struct format {
  Ctx &ctx_;
  template <class T>
  decltype(auto) join(const T &t) {
    if constexpr (vector_instance<T>) {
      for (const auto &el : t) {
        fmt(el);
        if (&el != &*(t.end() - 1)) { fmt(", "); }
      }
      return ctx_.out();
    } else if constexpr (tuple_instance<T>) {
      return std::apply(
          [&](const auto &x, const auto &...xs) { return fmt(x), ((fmt(" "), fmt(xs)), ...); }, t);
    } else {
      throw "can't be joined";
    }
  }
  template <class T>
  decltype(ctx_.out()) fmt(const T &t) {
    if constexpr (vector_instance<T>) {
      return fmt("["), join(t), fmt("]");
    } else if constexpr (variant_instance<T>) {
      return fmt("inj-"), std::visit(*this, t);
    } else if constexpr (tuple_instance<T>) {
      return fmt("("), join(t), fmt(")");
    } else if constexpr (std::same_as<std::decay_t<T>, char *>) {
      return std::formatter<std::string_view, CharT>{}.format(std::string_view{t}, ctx_);
    } else if constexpr (syntax_node<T>) {
      return fmt(type_name<T>), fmt(to_tpl(t));
    } else {
      return std::formatter<T, CharT>{}.format(t, ctx_);
    }
  }
  decltype(auto) operator()(const auto &t) { return fmt(t); }
};
template <class Ctx>
format(Ctx &ctx) -> format<typename Ctx::char_type, Ctx>;
}  // namespace stanly

template <class T, class CharT>
  requires stanly::tuple_instance<T> || stanly::vector_instance<T> || stanly::variant_instance<T> ||
           stanly::syntax_node<T>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const T &t, Ctx &ctx) const {
    return stanly::format{ctx}(t);
  }
};

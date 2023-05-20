#pragma once

#include <concepts>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "stanly-utils.h"

namespace stanly {

template <class Repr>
struct lang {
  // clang-format off
  struct update { Repr tgt; Repr field; Repr src; };
  struct load   { Repr var; Repr src; Repr field; };
  struct lit    { Repr var; Repr value; };
  struct alloc  { Repr var; Repr type; };
  struct ref    { Repr var; Repr src; };
  // clang-format on
  using node = std::variant<update, load, lit, alloc, ref>;
};

class idx {
 public:
  explicit operator size_t() const { return value; };
  using repr = uint16_t;
  explicit constexpr idx(size_t i) : value{static_cast<repr>(i)} {
    stanly_assert(i < (std::numeric_limits<idx::repr>::max() - 1),
                  std::format("{}-byte index can't support more than {} elements.",
                              sizeof(idx::repr), (std::numeric_limits<idx::repr>::max() - 2)));
  };
  constexpr idx() : value{0} {};
  constexpr bool operator<=>(const idx &other) const = default;

 private:
  repr value;
};
constexpr idx operator""_i(unsigned long long i) { return idx(i); }

template <class T>
struct is_syntax_node {
  constexpr static bool value = false;
};

template <class T>
concept syntax_node = is_syntax_node<T>::value;

template <class T>
concept syntax = all<is_syntax_node, std::decay_t<T>> &&
                 requires(T t) { std::visit([](auto &&) { return 1; }, t); };

template <class T>
struct is_syntax {
  constexpr static bool value = syntax<T>;
};

const int kN_BYTES_PACKED = 8;
template <class T>
concept packed_syntax = syntax<T> && sizeof(std::declval<T>()) <= kN_BYTES_PACKED;

template <syntax_node X, syntax_node Y>
bool operator==(X &&x, Y &&y) {
  if constexpr (std::same_as<X, Y>) {
    return to_tpl(std::forward<X>(x)) == to_tpl(std::forward<Y>(y));
  }
  return false;
};
template <syntax S>
bool operator==(S &&s1, S &&s2) {
  return std::visit(std::equal_to{}, std::forward<S>(s1), std::forward<S>(s2));
}

template <class T>
  requires contains<lang<idx>::node, std::decay_t<T>> ||
           contains<lang<std::string_view>::node, std::decay_t<T>>
struct is_syntax_node<T> {
  constexpr static bool value = true;
};

template <class T>
concept formatted_type =
    instance_of<T, std::vector> || instance_of<T, std::tuple> || instance_of<T, std::variant> ||
    instance_of<T, std::unordered_map> || syntax_node<T>;
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
      if constexpr (!std::same_as<T, std::tuple<>>) {
        std::apply([this](const auto &x, const auto &...xs) { fmt(x), ((fmt(" "), fmt(xs)), ...); },
                   t);
      }
      fmt(")");
    } else if constexpr (instance_of<T, std::unordered_map>) {
      fmt("{");
      for (const auto &el : t) {
        auto &[key, val] = el;
        if (&el != &*(t.begin())) { fmt(", "); }
        fmt(key);
        fmt(": ");
        fmt(val);
      }
      fmt("}");
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

template <>
struct std::hash<stanly::idx> {
  auto operator()(const stanly::idx &idx) const {
    using cast_to = size_t;
    static_assert(std::unsigned_integral<cast_to>);
    static_assert(std::unsigned_integral<stanly::idx::repr>);
    static_assert(sizeof(cast_to) >= sizeof(stanly::idx::repr));
    return std::hash<cast_to>{}(static_cast<cast_to>(idx));
  }
};

template <class CharT>
struct std::formatter<stanly::idx, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const stanly::idx &idx, Ctx &ctx) const {
    return std::formatter<std::size_t, CharT>{}.format(static_cast<std::size_t>(idx), ctx);
  }
};

template <stanly::formatted_type T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const T &t, auto &ctx) const { return stanly::format{&ctx}(t); }
};
#pragma once

#include <format>
#include <string_view>
#include <variant>

#include "handle.h"
#include "stanly-utils.h"
#include "to_tpl.h"

namespace stanly {
namespace syntax {
// clang-format off
struct alloc  { handle var; handle type; };
struct lit    { handle var; handle type; handle value; };
struct ref    { handle var; handle src; };
struct update { handle tgt; handle field; handle src; };
struct load   { handle var; handle src; handle field; };
// clang-format on
using ast_node = std::variant<update, load, lit, alloc, ref>;
static_assert(sizeof(std::declval<ast_node>()) == 8);

template <class T>
concept ast_cons = contains<syntax::ast_node, std::decay_t<T>>;

template <ast_cons X, ast_cons Y>
bool operator==(X &&x, Y &&y) {
  if constexpr (std::same_as<X, Y>) {
    return to_tpl(std::forward<X>(x)) == to_tpl(std::forward<Y>(y));
  }
  return false;
};
}  // namespace syntax

template <class T>
  requires syntax::ast_cons<T> || std::same_as<syntax::ast_node, std::decay_t<T>>
auto &operator<<(auto &s, const T &x) {
  return s << std::format("{}", x);
}
}  // namespace stanly

template <stanly::syntax::ast_cons T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const T x, auto &ctx) const {
    std::formatter<std::string_view, CharT>::format(stanly::type_name<T>, ctx);
    using tpl_type = std::decay_t<decltype(to_tpl(x))>;
    return static_cast<std::formatter<tpl_type>>(*this).format(to_tpl(x), ctx);
  }
};

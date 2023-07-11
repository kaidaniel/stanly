#pragma once

#include <format>
#include <string_view>
#include <variant>

#include "handle.h"
#include "stanly-utils.h"
#include "to_tpl.h"

namespace stanly::syntax {
// clang-format off
struct alloc  { handle var; handle type;                };
struct top    { handle var; handle reason;              };
struct lit    { handle var; handle type;  handle value; };
struct ref    { handle var; handle src;                 };
struct copy   { handle var; handle src;                 };

struct update { handle var; handle field; handle src;   };
struct append { handle var; handle src;                 };
struct load   { handle var; handle src;   handle field; };
struct merge  { handle var; handle old;   handle niu;   };
struct call   { handle var; handle fn;    handle arg;   };
// clang-format on
using ast_node = std::variant<alloc, top, lit, ref, copy, update, append, load, merge, call>;
static_assert(sizeof(std::declval<ast_node>()) == 8);
static_assert(requires(ast_node n) { std::visit([](auto inj) { return inj.var; }, n); });

template <class T>
concept ast_cons = contains<syntax::ast_node, std::decay_t<T>>;

template <ast_cons X, ast_cons Y>
bool
operator==(X &&x, Y &&y) {
  if constexpr (std::same_as<X, Y>) {
    return to_tpl(std::forward<X>(x)) == to_tpl(std::forward<Y>(y));
  }
  return false;
};

template <class T>
  requires stanly::syntax::ast_cons<T> || std::same_as<stanly::syntax::ast_node, std::decay_t<T>>
auto &
operator<<(auto &s, const T &x) {
  return s << std::format("{}", x);
}
}  // namespace stanly::syntax

template <stanly::syntax::ast_cons T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const T x, auto &ctx) const {
    std::formatter<std::string_view, CharT>::format(stanly::type_name<T>, ctx);
    using tpl_type = std::decay_t<decltype(to_tpl(x))>;
    return static_cast<std::formatter<tpl_type>>(*this).format(to_tpl(x), ctx);
  }
};

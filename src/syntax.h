#pragma once

#include <format>
#include <string_view>
#include <variant>

#include "handle.h"
#include "stanly-utils.h"
#include "to_tpl.h"

namespace stanly {
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
struct dcall  { handle var; handle fn;    handle arg;   };
// clang-format on
using node = std::variant<alloc, top, lit, ref, copy, update, append, load, merge, dcall>;
static_assert(sizeof(std::declval<node>()) == 8);
static_assert(requires(node n) { std::visit([](auto inj) { return inj.var; }, n); });

// clang-format off
struct branch { handle if_true; handle if_false; };
struct loop   { handle body; handle afterwards; };
// clang-format on
struct basic_block {
  using jump_targets = std::variant<branch, loop>;
  jump_targets next;
  std::vector<node> nodes;
};

template <class T>
concept ast_cons = contains<node, std::decay_t<T>>;

template <class T>
concept basic_block_cons = contains<basic_block::jump_targets, std::decay_t<T>>;

template <ast_cons X, ast_cons Y>
bool
operator==(X &&x, Y &&y) {
  if constexpr (std::same_as<X, Y>) {
    return to_tpl(std::forward<X>(x)) == to_tpl(std::forward<Y>(y));
  }
  return false;
};
template <class T>
  requires stanly::ast_cons<T> || std::same_as<stanly::node, std::decay_t<T>>
auto &
operator<<(auto &s, const T &x) {
  return s << std::format("{}", x);
}
}  // namespace stanly

template <stanly::ast_cons T, class CharT>
struct std::formatter<T, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const T x, auto &ctx) const {
    std::formatter<std::string_view, CharT>::format(stanly::type_name<T>, ctx);
    using tpl_type = std::decay_t<decltype(to_tpl(x))>;
    return static_cast<std::formatter<tpl_type>>(*this).format(to_tpl(x), ctx);
  }
};

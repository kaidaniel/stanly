#pragma once

#include "stanly-format.h"
#include "syntax.h"

namespace stanly::firstorder {
auto operator==(auto &&x, auto &&y) -> decltype(stanly::operator==(x, y)) {
  return stanly::operator==(x, y);
};

auto &operator<<(auto &s, const formatted_type auto &x) { return s << std::format("{}", x); }
template <class Repr>

struct syntax {
  // clang-format off
  struct update { Repr tgt; Repr field; Repr src; };
  struct load   { Repr var; Repr src; Repr field; };
  struct text   { Repr var; Repr literal; };
  struct alloc  { Repr var; Repr type; };
  struct ref    { Repr var; Repr src; };
  // clang-format on
  using node = std::variant<update, load, text, alloc, ref>;
};
}  // namespace stanly::firstorder

namespace stanly {
template <class T>
  requires contains<firstorder::syntax<idx>::node, std::decay_t<T>> ||
           contains<firstorder::syntax<std::string_view>::node, std::decay_t<T>>
struct is_syntax_node<T> {
  constexpr static bool value = true;
};

}  // namespace stanly

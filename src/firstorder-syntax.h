#pragma once
/*
#include <string_view>

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "language.h"
*/
#include <ostream>
#include <variant>
#include <vector>

#include "stanly-format.h"
#include "syntax.h"
namespace stanly {
/*
template <typename D>
concept abstract_domain =
    std::derived_from<
        typename D::domain, sparta::AbstractDomain<typename D::domain>> &&
    requires { typename D::domain; };
*/
}
namespace stanly::firstorder {
/*
using Kind = sparta::AbstractValueKind;
/// Abstraction of the program state (Var -> Value).
struct domain {
  using str = sparta::ConstantAbstractDomain<std::string_view>;
  using record = sparta::HashedSetAbstractDomain<std::string_view>;
  struct object : public sparta::DirectProductAbstractDomain<object, str, record> {
    using sparta::DirectProductAbstractDomain<object, str, record>::DirectProductAbstractDomain;
  };
  using env = sparta::HashedAbstractEnvironment<str, object>;
};
static_assert(::stanly::abstract_domain<abstract_domain>);
*/
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

namespace firstorder {
auto operator==(auto &&x, auto &&y) -> decltype(stanly::operator==(x, y)) {
  return stanly::operator==(x, y);
};

auto &operator<<(auto &s, const formatted_type auto &x) { return s << std::format("{}", x); }

}  // namespace firstorder

}  // namespace stanly

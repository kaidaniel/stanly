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
  using record_repr =
      std::conditional_t<std::same_as<Repr, std::string_view>, std::vector<Repr>, Repr>;
  // clang-format off
  struct store  { Repr target; Repr field; Repr src; };
  struct load   { Repr var; Repr src; Repr field; };
  struct text   { Repr var; Repr literal; };
  struct record { Repr var; record_repr record{}; };
  struct ref    { Repr var; Repr src; };
  struct top    { Repr var; Repr literal; };
  // clang-format on
  using node = std::variant<store, load, text, record, ref, top>;
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
auto operator==(auto&& x, auto&& y) -> decltype(stanly::operator==(x, y)) {
  return stanly::operator==(x, y);
};
namespace detail {
struct static_assertions_string_view : syntax<std::string_view> {
  static_assert(all<is_syntax_node, std::variant<load, const text&, text&&, top&>>);
  static_assert(all<is_syntax, std::variant<node, node&&, const node, const node&, const node&&>>);
  static_assert(requires(node node) { std::cout << node; });
  static_assert(requires(store store) { std::cout << store; });
};
struct static_assertions_idx : syntax<idx> {
  static_assert(all<is_syntax_node, std::variant<load, const text&, text&&, top&>>);
  static_assert(all<is_syntax, std::variant<node, node&&, const node, const node&, const node&&>>);
  static_assert(requires(node node) { std::cout << node; });
  static_assert(requires(store store) { std::cout << store; });
};
static_assert(packed_syntax<syntax<idx>::node>);
static_assert(packed_syntax<const syntax<idx>::node&>);
}  // namespace detail

}  // namespace firstorder

}  // namespace stanly

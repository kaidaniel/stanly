#pragma once
/*
#include <string_view>

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "language.h"
*/
#include <variant>
#include <vector>

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
  struct record { Repr var; record_repr record; };
  struct ref    { Repr var; Repr src; };
  struct top    { Repr var; Repr literal; };
  // clang-format on
  using node = std::variant<store, load, text, record, ref, top>;
};
}  // namespace stanly::firstorder
namespace stanly {
template <class T>
  requires contains<firstorder::syntax<idx>::node, T>
struct is_syntax_node<T> {
  constexpr static bool value = true;
};
template <class T>
  requires contains<firstorder::syntax<std::string_view>::node, T>
struct is_syntax_node<T> {
  constexpr static bool value = true;
};

static_assert(packed_syntax<firstorder::syntax<idx>::node>);
static_assert(syntax<firstorder::syntax<std::string_view>::node>);
}  // namespace stanly

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
struct abstract_domain {
  using string = sparta::ConstantAbstractDomain<std::string_view>;
  using record = sparta::HashedSetAbstractDomain<std::string_view>;
  struct string_x_record : public sparta::DirectProductAbstractDomain<
                               string_x_record, string, record> {
    using Product =
        sparta::DirectProductAbstractDomain<string_x_record, string, record>;
    using Product::DirectProductAbstractDomain;
  };
  using domain = sparta::HashedAbstractEnvironment<string, string_x_record>;
};
static_assert(::stanly::abstract_domain<abstract_domain>);
*/
template <class Repr>
struct syntax {
  // clang-format off
  using record_repr = std::conditional_t<std::same_as<Repr, std::string_view>, std::vector<Repr>, Repr>;
  struct store   { Repr target; Repr field; Repr src; };
  struct load  { Repr var; Repr src; Repr field; };
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

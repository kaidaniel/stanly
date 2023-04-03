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
  struct set_field   { Repr rhs; Repr target; Repr field; };
  struct load_field  { Repr lhs; Repr source; Repr field; };
  struct load_text   { Repr lhs; Repr literal; };
  struct load_record { Repr lhs; std::vector<Repr> literal; };
  struct load_var    { Repr lhs; Repr rhs; };
  struct load_top    { Repr lhs; Repr literal; };
  // clang-format on
  using repr = Repr;
  using node = std::variant<set_field, load_field, load_text, load_record, load_var, load_top>;
};
static_assert(stanly::syntax<firstorder::syntax<stanly::idx>>);

}  // namespace stanly::firstorder

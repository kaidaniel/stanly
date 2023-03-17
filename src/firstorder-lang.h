#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "language.h"
#include "metaprogramming.h"
#include <string_view>
#include <variant>
#include <vector>

namespace stanly {
template <typename D>
concept abstract_domain =
    std::derived_from<
        typename D::domain, sparta::AbstractDomain<typename D::domain>> &&
    requires { typename D::domain; };
} // namespace stanly

namespace stanly::firstorder {
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

struct language {
  template <class Repr, class Record = std::vector<Repr>> struct nodes {
    using repr = Repr;
    using record = Record;
    // clang-format off
    struct set_field { Repr rhs; Repr target; Repr field; };
    struct load_field { Repr lhs; Repr source; Repr field; };
    struct load_text { Repr lhs; Repr literal; };
    struct load_record { Repr lhs; Repr literal; };
    struct load_var { Repr lhs; Repr rhs; };
    struct load_top { Repr lhs; Repr literal;};
    // clang-format on
  };
  template <class Repr>
  using typelist = metaprogramming::TypeList<
#define T typename nodes<Repr>::
      T set_field, T load_field, T load_text, T load_record, T load_var,
      T load_top
#undef T
      >;
};
static_assert(::stanly::language<language>);
} // namespace stanly::firstorder
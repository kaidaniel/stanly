#pragma once

#include <concepts>

#include "AbstractDomain.h"
#include "ConstantAbstractDomain.h"
#include "FiniteAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedAbstractPartition.h"
#include "HashedSetAbstractDomain.h"
// import order is important for operator<< lookup.
// clang-format off
#include "DisjointUnionAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
// clang-format on

#include "handle.h"

namespace stanly {

// state: memory x scope
// memory: address -> object  partition (implicit bottom)
// scope: var -> [address]    env (implicit top; single bottom implies scope bottom)
// object: type x value
// value: record + constant
// record: bindings x [fields]
// bindings: field -> [address]    partition
// type: dict, dataframe, closure, int, top, ...vw
// var, address, field: int

namespace detail {
using namespace sparta;
template <class T>
concept abstract_domain = std::derived_from<T, AbstractDomain<T>>;
enum class RowVarEls { Bot, Closed, Open };
std::ostream& operator<<(std::ostream& os, RowVarEls rve);
using enum RowVarEls;
using row_var_l = BitVectorLattice<RowVarEls, 3>;
static row_var_l l_({Bot, Closed, Open}, {{Bot, Closed}, {Closed, Open}});
template <class Repr>
struct abstract_domain_types {
  using field_repr = Repr;
  using address_repr = Repr;
  using var_repr = Repr;
  using constant_repr = Repr;
  using type_repr = Repr;
  using row_var = FiniteAbstractDomain<RowVarEls, row_var_l, row_var_l::Encoding, &l_>;
  using addresses = HashedSetAbstractDomain<address_repr>;
  using constant = ConstantAbstractDomain<constant_repr>;
  using defined = HashedAbstractPartition<field_repr, addresses>;
  using used = HashedSetAbstractDomain<field_repr>;
  struct record : DirectProductAbstractDomain<record, row_var, defined, used> {
    using DirectProductAbstractDomain<record, row_var, defined, used>::DirectProductAbstractDomain;
  };
  using data = DisjointUnionAbstractDomain<record, constant>;
  using type = ConstantAbstractDomain<type_repr>;
  struct object : DirectProductAbstractDomain<object, type, data> {
    using DirectProductAbstractDomain<object, type, data>::DirectProductAbstractDomain;
  };

  using scope = HashedAbstractEnvironment<var_repr, addresses>;
  using memory = HashedAbstractPartition<address_repr, object>;
  struct state : DirectProductAbstractDomain<state, scope, memory> {
    using DirectProductAbstractDomain<state, scope, memory>::DirectProductAbstractDomain;
    using dp = DirectProductAbstractDomain<state, scope, memory>;
    template <class T>
      requires std::same_as<T, memory> || std::same_as<T, scope>
    constexpr static int idx = std::same_as<T, memory> ? 1 : 0;
    template <class Target>
    void set_key(
        const std::conditional_t<std::same_as<Target, memory>, address_repr, var_repr>& index,
        const std::conditional_t<std::same_as<Target, memory>, object, addresses>& value) {
      dp::template apply<idx<Target>>([&](Target* t) { t->set(index, value); });
    }
    template <class Target>
    void join(const Target& t) {
      dp::template apply<idx<Target>>([&](Target* old) { old->join_with(std::move(t)); });
    };
  };
  using kind = AbstractValueKind;
  struct bot_type {
    template <class T>
      requires requires { T::bottom(); }
    operator T() const {
      return T::bottom();
    }
  } bot{};
  struct top_type {
    template <class T>
      requires requires { T::top(); }
    operator T() const {
      return T::top();
    }
  } top{};
  static_assert(std::derived_from<state, AbstractDomain<state>>);
  static_assert(std::derived_from<scope, AbstractDomain<scope>>);
  static_assert(std::derived_from<memory, AbstractDomain<memory>>);
};
}  // namespace detail
template <class T>
using domains = detail::abstract_domain_types<T>;
template <class T>
using domain = domains<T>::state;
}  // namespace stanly

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

#include "repr.h"

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
struct abstract_domain_types {
  using field_repr = repr;
  using address_repr = repr;
  using var_repr = repr;
  using constant_repr = repr;
  using type_repr = repr;
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
    template <class F>
      requires requires(F f, used* u) { f(u); } || requires(F f, defined* d) { f(d); }
    void apply_to_record(var_repr var, F&& f) {
      dp::template apply<idx<memory>>([&](memory* m) {
        m->update(var, [&](object* o) {
          o->template apply<1>([&](data* d) {
            d->template apply<record>(
                [&](record* r) { r->template apply<requires(used* u) { f(u); } ? 2 : 1>(f); });
          });
        });
      });
    }
    void add_used_field(var_repr var, field_repr field) {
      apply_to_record(var, [field](used* u) { u->add(field); });
    }
    void define_field(var_repr src, field_repr field, address_repr tgt) {
      apply_to_record(src, [field, tgt](defined* d) { d->set(field, addresses{tgt}); });
    }
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
using domains = detail::abstract_domain_types;
template <class T>
using domain = domains::state;
}  // namespace stanly

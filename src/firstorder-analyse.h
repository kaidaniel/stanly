#include <AbstractDomain.h>
#include <ConstantAbstractDomain.h>
#include <DisjointUnionAbstractDomain.h>
#include <HashedAbstractPartition.h>

#include <concepts>

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"

namespace stanly::firstorder {

// reduced prod: intersection of denot. single bottom implies prod bottom. reduction is called e.g.
// in if-statements. maybe also fn args? direct prod: prod. bottom implies all bottom. Domain:
// memory x scope memory: address -> object  partition (implicit bottom; single top does _not_ imply
// partition top) scope: var -> [address]    env (implicit top; single bottom implies scope bottom)
// var, address, field: int          const
// [x]: set[x]                powerset of integers
// object: type x value       direct-prod (or: reduced prod for type-value mismatch)
// value: record + constant   disjoint-sum
// record: def bindings x use [fields]   direct-prod
// bindings: field -> [address]    partition
// type: dict, dataframe, closure, int, top, ...

template <class T>
concept abstract_domain = std::derived_from<T, sparta::AbstractDomain<T>>;

template <class Field = short, class Address = short, class Var = short, class Constant = short,
          class Type = short>
struct abstract_domain_types {
  struct concrete {
    using field = Field;
    using address = Address;
    using var = Var;
    using constant = Constant;
    using type = Type;
  };
  using addresses = sparta::HashedSetAbstractDomain<Address>;
  using constant = sparta::ConstantAbstractDomain<Constant>;
  using bindings = sparta::DisjointUnionAbstractDomain<Field, addresses>;
  using fields = sparta::HashedSetAbstractDomain<Field>;
  struct record : sparta::DirectProductAbstractDomain<record, bindings, fields> {};
  using value = sparta::DisjointUnionAbstractDomain<record, constant>;
  using type = sparta::ConstantAbstractDomain<Type>;
  struct object : sparta::DirectProductAbstractDomain<object, type, value> {};

  using scope = sparta::HashedAbstractEnvironment<Var, addresses>;
  using memory = sparta::HashedAbstractPartition<Address, object>;
  struct state : sparta::DirectProductAbstractDomain<state, scope, memory> {};
  static_assert(abstract_domain<state>);
  static_assert(abstract_domain<scope>);
  static_assert(abstract_domain<memory>);
};

// using abstract_domain = abstract_domain_types<>::state;
}  // namespace stanly::firstorder

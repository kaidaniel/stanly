#include <AbstractDomain.h>
#include <ConstantAbstractDomain.h>
#include <DisjointUnionAbstractDomain.h>
#include <HashedAbstractPartition.h>

#include <concepts>

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"

namespace stanly {

// state: memory x scope
// memory: address -> object  partition (implicit bottom)
// scope: var -> [address]    env (implicit top; single bottom implies scope bottom)
// object: type x value
// value: record + constant
// record: bindings x [fields]
// bindings: field -> [address]    partition
// type: dict, dataframe, closure, int, top, ...
// var, address, field: int

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
  struct record : sparta::DirectProductAbstractDomain<record, bindings, fields> {
    using sparta::DirectProductAbstractDomain<record, bindings,
                                              fields>::DirectProductAbstractDomain;
  };
  using value = sparta::DisjointUnionAbstractDomain<record, constant>;
  using type = sparta::ConstantAbstractDomain<Type>;
  struct object : sparta::DirectProductAbstractDomain<object, type, value> {
    using sparta::DirectProductAbstractDomain<object, type, value>::DirectProductAbstractDomain;
  };

  using scope = sparta::HashedAbstractEnvironment<Var, addresses>;
  using memory = sparta::HashedAbstractPartition<Address, object>;
  struct state : sparta::DirectProductAbstractDomain<state, scope, memory> {
    using sparta::DirectProductAbstractDomain<state, scope, memory>::DirectProductAbstractDomain;
  };
  static_assert(abstract_domain<state>);
  static_assert(abstract_domain<scope>);
  static_assert(abstract_domain<memory>);
};

// using abstract_domain = abstract_domain_types<>::state;
}  // namespace stanly

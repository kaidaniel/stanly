#include <AbstractDomain.h>
#include <ConstantAbstractDomain.h>
#include <DisjointUnionAbstractDomain.h>
#include <HashedAbstractPartition.h>

#include <concepts>

#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "handle.h"

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

namespace detail {
template <class T>
concept abstract_domain = std::derived_from<T, sparta::AbstractDomain<T>>;

template <class DefaultRepr = handle, class Field = DefaultRepr, class Address = DefaultRepr,
          class Var = DefaultRepr, class Constant = DefaultRepr, class Type = DefaultRepr>
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
  using defined = sparta::HashedAbstractPartition<Field, addresses>;
  using used = sparta::HashedSetAbstractDomain<Field>;
  struct record : sparta::DirectProductAbstractDomain<record, defined, used> {
    using sparta::DirectProductAbstractDomain<record, defined, used>::DirectProductAbstractDomain;
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
  using domain = state;
  using kind = sparta::AbstractValueKind;
  struct bottom_type {
    template <class T>
      requires requires { T::bottom(); }
    operator T() const {
      return T::bottom();
    }
  } bottom{};
  struct top_type {
    template <class T>
      requires requires { T::top(); }
    operator T() const {
      return T::top();
    }
  } top{};
  static_assert(abstract_domain<state>);
  static_assert(abstract_domain<scope>);
  static_assert(abstract_domain<memory>);
};
}  // namespace detail

using domains = detail::abstract_domain_types<std::string_view>;
using packed_domains = detail::abstract_domain_types<handle>;
}  // namespace stanly

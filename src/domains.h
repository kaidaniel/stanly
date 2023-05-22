#include <concepts>

// clang-format off
#include "FiniteAbstractDomain.h"
#include "AbstractDomain.h"
#include "ConstantAbstractDomain.h"
#include "DisjointUnionAbstractDomain.h"
#include "HashedAbstractPartition.h"
#include "HashedAbstractEnvironment.h"
#include "DirectProductAbstractDomain.h"
#include "HashedSetAbstractDomain.h"
#include "handle.h"
// clang-format on

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
using namespace sparta;
template <class T>
concept abstract_domain = std::derived_from<T, AbstractDomain<T>>;
enum class RowVarEls { Bot, Closed, Open };
std::ostream& operator<<(std::ostream& os, RowVarEls rve) {
  switch (rve) {
    case RowVarEls::Bot: os << "Bot"; break;
    case RowVarEls::Closed: os << "Closed"; break;
    case RowVarEls::Open: os << "Open"; break;
  };
  return os;
}
using enum RowVarEls;
using row_var_l = BitVectorLattice<RowVarEls, 3>;
row_var_l l_({Bot, Closed, Open}, {{Bot, Closed}, {Closed, Open}});
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
  using row_var = FiniteAbstractDomain<RowVarEls, row_var_l, row_var_l::Encoding, &l_>;
  using addresses = HashedSetAbstractDomain<Address>;
  using constant = ConstantAbstractDomain<Constant>;
  using defined = HashedAbstractPartition<Field, addresses>;
  using used = HashedSetAbstractDomain<Field>;
  struct record : DirectProductAbstractDomain<record, row_var, defined, used> {
    using DirectProductAbstractDomain<record, row_var, defined, used>::DirectProductAbstractDomain;
    record(defined&& d, RowVarEls rv = Bot) : record({row_var{rv}, std::move(d), used::bottom()}) {}
    record(used&& u, RowVarEls rv = Bot) : record({row_var{rv}, defined::bottom(), std::move(u)}) {}
  };
  using data = DisjointUnionAbstractDomain<record, constant>;
  using type = ConstantAbstractDomain<Type>;
  struct object : DirectProductAbstractDomain<object, type, data> {
    using DirectProductAbstractDomain<object, type, data>::DirectProductAbstractDomain;
    object(defined&& d, type&& t = type::bottom()) : object({std::move(t), data{std::move(d)}}) {}
    object(used&& d, type&& t = type::bottom()) : object({std::move(t), data{std::move(d)}}) {}
    object(constant&& c, type&& t = type::bottom()) : object({std::move(t), data{std::move(c)}}) {}
  };

  using scope = HashedAbstractEnvironment<Var, addresses>;
  using memory = HashedAbstractPartition<Address, object>;
  struct state : DirectProductAbstractDomain<state, scope, memory> {
    using DirectProductAbstractDomain<state, scope, memory>::DirectProductAbstractDomain;
    using dp = DirectProductAbstractDomain<state, scope, memory>;
    void set_mem(Address&& a, object&& o) {
      dp::template apply<1>([&](memory* m) { m->set(a, std::move(o)); });
    }
    void set_var(Var&& v, addresses&& a) {
      dp::template apply<0>([=](scope* s) { s->set(v, std::move(a)); });
    }
    void join_mem(memory&& mem) {
      dp::template apply<1>([&](memory* m) { m->join_with(std::move(mem)); });
    }
  };
  using domain = state;
  using kind = AbstractValueKind;
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

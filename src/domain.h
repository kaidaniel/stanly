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
#include "stanly-utils.h"

namespace stanly::domains {

// state: memory x scope
// memory: address -> object  partition (implicit bottom)
// scope: var -> [address]    env (implicit top; single bottom implies scope bottom)
// object: type x value
// value: record + constant
// record: bindings x [fields]
// bindings: field -> [address]    partition
// type: dict, dataframe, closure, int, top, ...vw
// var, address, field: int
using namespace sparta;
template <class T>
concept abstract_domain = std::derived_from<T, AbstractDomain<T>>;
enum class RowVarEls { Bot, Closed, Open };
std::ostream& operator<<(std::ostream& os, RowVarEls rve);
using enum RowVarEls;
using row_var_l = BitVectorLattice<RowVarEls, 3>;
static row_var_l l_({Bot, Closed, Open}, {{Bot, Closed}, {Closed, Open}});
using field_repr = handle;
using address_repr = handle;
using var_repr = handle;
using constant_repr = handle;
using type_repr = handle;
using row_var = FiniteAbstractDomain<RowVarEls, row_var_l, row_var_l::Encoding, &l_>;
using addresses = HashedSetAbstractDomain<address_repr>;
using constant = ConstantAbstractDomain<constant_repr>;
using defined = HashedAbstractPartition<field_repr, addresses>;
using used = HashedSetAbstractDomain<field_repr>;
struct record : DirectProductAbstractDomain<record, row_var, defined, used> {
  using DirectProductAbstractDomain<record, row_var, defined, used>::DirectProductAbstractDomain;
  template <class T>
    requires std::same_as<T, row_var> || std::same_as<T, defined> || std::same_as<T, used>
  constexpr static int idx = std::same_as<T, row_var>   ? 0
                             : std::same_as<T, defined> ? 1
                                                        : 2;
};
using data = DisjointUnionAbstractDomain<record, constant>;
using type = ConstantAbstractDomain<type_repr>;
struct object : DirectProductAbstractDomain<object, type, data> {
  using DirectProductAbstractDomain<object, type, data>::DirectProductAbstractDomain;
};

using scope = sparta::HashedAbstractEnvironment<var_repr, addresses>;
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
static const struct {
  template <class T>
    requires requires { T::bottom(); }
  operator T() const {
    return T::bottom();
  }
} bot{};
static const struct {
  template <class T>
    requires requires { T::top(); }
  operator T() const {
    return T::top();
  }
} top{};
static_assert(std::derived_from<state, AbstractDomain<state>>);
static_assert(std::derived_from<scope, AbstractDomain<scope>>);
static_assert(std::derived_from<memory, AbstractDomain<memory>>);
}  // namespace stanly::domains
namespace stanly {
using domain = domains::state;
}

template <class Repr, class CharT>
struct std::formatter<sparta::HashedSetAbstractDomain<Repr>, CharT>
    : std::formatter<std::string_view, CharT> {
  auto format(const sparta::HashedSetAbstractDomain<Repr>& hsad, auto& ctx) const {
    std::ostringstream oss{};
    oss << hsad;
    std::string str = oss.str();
    size_t pos = str.find(']');
    if (pos != std::string::npos) { str = str.substr(pos + 1); }
    return std::format_to(ctx.out(), "{}", str);
  }
};

template <class CharT>
struct std::formatter<stanly::domains::row_var, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::row_var& row_var, auto& ctx) const {
    using namespace stanly::domains;
    std::ostringstream oss{};
    ::operator<<(oss, row_var);
    std::string str = oss.str();
    return std::format_to(ctx.out(), "{}", oss.str());
  }
};

template <class CharT>
struct std::formatter<stanly::domains::record, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::record& record, auto& ctx) const {
    using namespace stanly::domains;
    return std::format_to(
        ctx.out(), "{}{}defined{} used{}{}", "<",
        (record.get<record::idx<row_var>>().element() == RowVarEls::Closed) ? "* " : "",
        record.get<record::idx<defined>>().bindings(), record.get<record::idx<used>>(), ">");
  }
};

template <class CharT>
struct std::formatter<stanly::domains::constant, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::constant& constant, auto& ctx) const {
    using namespace stanly::domains;
    std::ostringstream oss{};
    oss << constant;
    return std::format_to(ctx.out(), "{}", oss.str());
  }
};

struct visitor {
  using result_type = std::string;
  result_type operator()(auto&& x) const { return std::format("{}", x); };
};
template <class CharT>
struct std::formatter<stanly::domains::data, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::data& data, auto& ctx) const {
    using namespace stanly::domains;
    std::ostringstream oss{};
    ::operator<<(oss, data);
    return std::format_to(ctx.out(), "{}", data::apply_visitor(visitor{}, data));
  }
};

template <class CharT>
struct std::formatter<stanly::domains::object, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::object& object, auto& ctx) const {
    return std::format_to(ctx.out(), "({} {})", object.get<0>(), object.get<1>());
  }
};

template <class CharT>
struct std::formatter<stanly::domains::memory, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::memory& memory, auto& ctx) const {
    if (memory.is_top()) {
      return std::format_to(ctx.out(), "memory{}", "{∀ addr. addr: object(top)}");
    }
    if (memory.is_bottom()) {
      return std::format_to(ctx.out(), "memory{}", "{∀ addr. addr: unused}");
    }
    return std::format_to(ctx.out(), "memory({} else unused)", memory.bindings());
  }
};

template <class CharT>
struct std::formatter<stanly::domains::scope, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::scope& scope, auto& ctx) const {
    if (scope.is_top()) {
      return std::format_to(ctx.out(), "scope{}", "{∀ var. var: addresses(top)}");
    }
    if (scope.is_bottom()) { return std::format_to(ctx.out(), "scope{}", "{invalid}"); }
    return std::format_to(ctx.out(), "scope({} else addresses(top))", scope.bindings());
  }
};

template <class CharT>
struct std::formatter<stanly::domains::state, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const stanly::domains::state& state, auto& ctx) const {
    using namespace stanly::domains;
    std::ostringstream oss{};
    ::operator<<(oss, state.get<state::idx<scope>>());
    std::string scope_str = oss.str();
    return std::format_to(ctx.out(), "state({}, {})", state.get<state::idx<scope>>(),
                          state.get<state::idx<memory>>());
  }
};

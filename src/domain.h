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
    return std::format_to(ctx.out(), "{}", oss.str());
  }
};

template <class Record, class CharT>
  requires std::same_as<stanly::domains::record, Record>
struct std::formatter<Record, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const Record& record, auto& ctx) const {
    using namespace stanly::domains;
    return std::format_to(
        ctx.out(), "({}defined{}, used{})",
        (record.template get<record::idx<row_var>>().element() == RowVarEls::Closed) ? "* " : "",
        record.template get<record::idx<defined>>().bindings(),
        record.template get<record::idx<used>>());
  }
};

template <class Constant, class CharT>
  requires std::same_as<stanly::domains::constant, Constant>
struct std::formatter<Constant, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const Constant& constant, auto& ctx) const {
    using namespace stanly::domains;
    std::ostringstream oss{};
    oss << constant;
    return std::format_to(ctx.out(), "{}", oss.str());
  }
};

struct data_visitor {
  using result_type = std::string;
  result_type operator()(auto&& x) const { return std::format("{}", x); };
};
template <class Data, class CharT>
  requires std::same_as<stanly::domains::data, Data>
struct std::formatter<Data, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const Data& data, auto& ctx) const {
    using namespace stanly::domains;
    std::ostringstream oss{};
    ::operator<<(oss, data);
    return std::format_to(ctx.out(), "{}", data::apply_visitor(data_visitor{}, data));
  }
};

template <class Object, class CharT>
  requires std::same_as<stanly::domains::object, Object>
struct std::formatter<Object, CharT> : std::formatter<std::string_view, CharT> {
  auto format(const Object& object, auto& ctx) const {
    return std::format_to(ctx.out(), "({} {})", object.template get<0>(), object.template get<1>());
  }
};

template <class T>
struct with_handles {
  const T& t;
  const std::map<stanly::handle, std::string_view>& handles_to_str;
};

template <class T, class Any>
with_handles(const T&, const Any&) -> with_handles<T>;

template <class Repr, class CharT>
struct std::formatter<with_handles<sparta::HashedSetAbstractDomain<Repr>>, CharT>
    : std::formatter<std::string_view, CharT> {
  auto format(const with_handles<sparta::HashedSetAbstractDomain<Repr>>& hsad, auto& ctx) const {
    std::string str = "{";
    for (const auto& x : hsad.t.elements()) {
      str += std::format("{}, ", hsad.handles_to_str.at(x));
    }
    return std::format_to(ctx.out(), "{}", str.substr(0, str.size() - 2) + "}");
  }
};

template <class CharT>
struct std::formatter<with_handles<stanly::domains::object>, CharT>
    : stanly::detail::lines_arg_parser<std::formatter<std::string_view, CharT>> {
  auto format(auto&& s, auto& ctx) const {
    return std::format_to(ctx.out(), "{}", "with_handles<object>");
  }
};
template <class T>
std::unordered_map<std::string, std::string> format_bindings(const T& x) {
  std::unordered_map<std::string, std::string> out;
  if constexpr (requires { x.t.bindings(); }) {
    for (const auto& [key, value] : x.t.bindings()) {
      out[std::string{x.handles_to_str.at(key)}] =
          std::format("{}", with_handles{value, x.handles_to_str});
    }
  } else if constexpr (requires { x.bindings(); }) {
    for (const auto& [key, value] : x.bindings()) {
      out[std::format("{}", key)] = std::format("{}", value);
    }
  }
  return out;
}

template <class Memory, class CharT>
  requires std::same_as<stanly::domains::memory, Memory> ||
           std::same_as<with_handles<stanly::domains::memory>, Memory>
struct std::formatter<Memory, CharT>
    : stanly::detail::lines_arg_parser<std::formatter<std::string_view, CharT>> {
  auto format(const Memory& memory, auto& ctx) const {
    const stanly::domains::memory* mem = nullptr;
    if constexpr (std::same_as<stanly::domains::memory, Memory>) { mem = &memory; }
    if constexpr (std::same_as<with_handles<stanly::domains::memory>, Memory>) { mem = &memory.t; }
    if (mem->is_top()) {
      return std::format_to(ctx.out(), "memory{}", "{∀ addr. addr: object(top)}");
    }
    if (mem->is_bottom()) {
      return std::format_to(ctx.out(), "memory{}", "{∀ addr. addr: unused}");
    }
    if (this->lines_arg) {
      return std::format_to(ctx.out(), "memory{:lines}", format_bindings(memory));
    }
    return std::format_to(ctx.out(), "memory{}", format_bindings(memory));
  }
};

template <class Scope, class CharT>
  requires std::same_as<stanly::domains::scope, Scope> ||
           std::same_as<with_handles<stanly::domains::scope>, Scope>
struct std::formatter<Scope, CharT>
    : stanly::detail::lines_arg_parser<std::formatter<std::string_view, CharT>> {
  auto format(const Scope& scope, auto& ctx) const {
    const stanly::domains::scope* scp = nullptr;
    if constexpr (std::same_as<stanly::domains::scope, Scope>) { scp = &scope; }
    if constexpr (std::same_as<with_handles<stanly::domains::scope>, Scope>) { scp = &scope.t; }
    if (scp->is_top()) {
      return std::format_to(ctx.out(), "scope{}", "{∀ var. var: addresses(top)}");
    }
    if (scp->is_bottom()) { return std::format_to(ctx.out(), "scope{}", "{invalid}"); }
    if (this->lines_arg) {
      return std::format_to(ctx.out(), "scope{:lines}", format_bindings(scope));
    }
    return std::format_to(ctx.out(), "scope{}", format_bindings(scope));
  }
};

inline std::tuple<const stanly::domains::scope&, const stanly::domains::memory&>
get_scope_and_memory(const stanly::domains::state& state) {
  return {state.get<stanly::domains::state::idx<stanly::domains::scope>>(),
          state.get<stanly::domains::state::idx<stanly::domains::memory>>()};
}

inline std::tuple<with_handles<stanly::domains::scope>, with_handles<stanly::domains::memory>>
get_scope_and_memory(with_handles<stanly::domains::state> wstate) {
  const auto [scp, mem] = get_scope_and_memory(wstate.t);
  return {with_handles<stanly::domains::scope>{scp, wstate.handles_to_str},
          with_handles<stanly::domains::memory>{mem, wstate.handles_to_str}};
}

template <class State, class CharT>
  requires std::same_as<stanly::domains::state, State> ||
           std::same_as<with_handles<stanly::domains::state>, State>
struct std::formatter<State, CharT>
    : stanly::detail::lines_arg_parser<std::formatter<std::string_view, CharT>> {
  auto format(const State& state, auto& ctx) const {
    using namespace stanly::domains;
    const auto& [scp, mem] = get_scope_and_memory(state);
    if (this->lines_arg) {
      std::string scope_and_memory;

      for (auto c : std::format("\n{:lines},\n{:lines}", scp, mem)) {
        scope_and_memory += c;
        if (c == '\n') { scope_and_memory += "    "; }
      }
      return std::format_to(ctx.out(), "state({}\n)", scope_and_memory);
    }
    return std::format_to(ctx.out(), "state({}, {})", scp, mem);
  }
};

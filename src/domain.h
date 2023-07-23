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
#include "DirectProductAbstractDomain.h"
// clang-format on

#include "handle.h"
#include "stanly-utils.h"
#include "string-index.h"

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
using namespace sparta;
template <class T>
concept abstract_domain = std::derived_from<T, AbstractDomain<T>>;
enum class RowVarEls { Closed, Open };
std::ostream& operator<<(std::ostream& os, RowVarEls rve);
using enum RowVarEls;
using row_var_l = BitVectorLattice<RowVarEls, 2>;
static row_var_l l_({Closed, Open}, {{Closed, Open}});
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
struct data : DirectProductAbstractDomain<data, record, constant> {
  using DirectProductAbstractDomain<data, record, constant>::DirectProductAbstractDomain;
  template <class T>
    requires std::same_as<T, record> || std::same_as<T, constant>
  constexpr static int idx = std::same_as<T, record> ? 0 : 1;
};
using type = ConstantAbstractDomain<type_repr>;
struct object : DirectProductAbstractDomain<object, type, data> {
  using DirectProductAbstractDomain<object, type, data>::DirectProductAbstractDomain;
  template <class T>
    requires std::same_as<T, type> || std::same_as<T, data>
  constexpr static int idx = std::same_as<T, type> ? 0 : 1;
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
  void
  set_key(
      const std::conditional_t<std::same_as<Target, memory>, address_repr, var_repr>& index,
      const std::conditional_t<std::same_as<Target, memory>, object, addresses>& value) {
    dp::template apply<idx<Target>>([&](Target* t) { t->set(index, value); });
  }
};
static_assert(std::derived_from<state, AbstractDomain<state>>);
static_assert(std::derived_from<scope, AbstractDomain<scope>>);
static_assert(std::derived_from<memory, AbstractDomain<memory>>);
}  // namespace stanly

template <class T>
struct with_handles {
  const T& t;
  const std::map<stanly::handle, std::string_view>& handles_to_str =
      stanly::global_string_index.handles();
};

template <class T>
const T&
ref_to_t(const with_handles<T>& x) {
  return x.t;
}
template <class T>
const T&
ref_to_t(const T& x) {
  return x;
}

template <class T, class Any>
with_handles(const T&, const Any&) -> with_handles<T>;

template <class T>
with_handles(const T&) -> with_handles<T>;

template <class Repr, class CharT>
struct std::formatter<sparta::HashedSetAbstractDomain<Repr>, CharT>
    : std::formatter<std::string_view, CharT> {
  auto
  format(const sparta::HashedSetAbstractDomain<Repr>& hsad, auto& ctx) const {
    std::ostringstream oss{};
    oss << hsad;
    std::string str = oss.str();
    size_t pos = str.find(']');
    if (pos != std::string::npos) { str = str.substr(pos + 1); }
    return std::format_to(ctx.out(), "{}", str);
  }
};

template <class Repr, class CharT>
struct std::formatter<with_handles<sparta::HashedSetAbstractDomain<Repr>>, CharT>
    : std::formatter<std::string_view, CharT> {
  auto
  format(const with_handles<sparta::HashedSetAbstractDomain<Repr>>& hsad, auto& ctx) const {
    switch (hsad.t.kind()) {
      case sparta::AbstractValueKind::Top: [[fallthrough]];
      case sparta::AbstractValueKind::Bottom: {
        return std::format_to(ctx.out(), "{}", hsad.t);
        break;
      }
      case sparta::AbstractValueKind::Value: {
        std::string str = "{";
        for (const auto& x : hsad.t.elements()) {
          str += std::format("{}, ", hsad.handles_to_str.at(x));
        }
        return std::format_to(ctx.out(), "{}", str.substr(0, str.size() - 2) + "}");
      }
    }
  }
};

template <class CharT>
struct std::formatter<stanly::row_var, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const stanly::row_var& row_var, auto& ctx) const {
    std::ostringstream oss{};
    ::operator<<(oss, row_var);
    return std::format_to(ctx.out(), "{}", oss.str());
  }
};

template <class T>
std::unordered_map<std::string, std::string>
format_bindings(const T& x) {
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

template <class Record, class CharT>
  requires std::same_as<stanly::record, Record>
struct std::formatter<Record, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const Record& record, auto& ctx) const {
    using namespace stanly;
    return std::format_to(
        ctx.out(), "({}defined{}, used{})",
        (record.template get<record::idx<row_var>>().element() == RowVarEls::Open) ? "* " : "",
        record.template get<record::idx<defined>>().bindings(),
        record.template get<record::idx<used>>());
  }
};

template <class CharT>
struct std::formatter<with_handles<stanly::record>, CharT>
    : std::formatter<std::string_view, CharT> {
  auto
  format(const with_handles<stanly::record>& record, auto& ctx) const {
    using namespace stanly;
    std::string s_used = std::format(
        "{}",
        with_handles<used>{record.t.template get<record::idx<used>>(), record.handles_to_str});
    return std::format_to(
        ctx.out(), "({}defined{}, used{})",
        (record.t.template get<record::idx<row_var>>().element() == RowVarEls::Open) ? "* " : "",
        format_bindings(
            with_handles{record.t.template get<record::idx<defined>>(), record.handles_to_str}),
        // with_handles<used>{record.t.template get<record::idx<used>>(), record.handles_to_str});
        s_used);
  }
};

template <class Constant, class CharT>
  requires std::same_as<stanly::constant, Constant> ||
           std::same_as<with_handles<stanly::constant>, Constant>
struct std::formatter<Constant, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const Constant& cnst, auto& ctx) const {
    using namespace stanly;
    std::ostringstream oss{};
    if constexpr (std::same_as<constant, Constant>) { oss << cnst; }
    if constexpr (std::same_as<with_handles<constant>, Constant>) {
      switch (cnst.t.kind()) {
        case sparta::AbstractValueKind::Top: {
          [[fallthrough]];
        }
        case sparta::AbstractValueKind::Bottom: {
          oss << cnst.t;
          break;
        }
        case sparta::AbstractValueKind::Value: {
          oss << cnst.handles_to_str.at(*(cnst.t.get_constant()));
          break;
        }
      }
    }
    return std::format_to(ctx.out(), "{}", oss.str());
  }
};

template <class Data, class CharT>
  requires std::same_as<stanly::data, Data> || std::same_as<with_handles<stanly::data>, Data>
struct std::formatter<Data, CharT> : std::formatter<std::string_view, CharT> {
  struct data_visitor {
    using result_type = std::string;
    const std::map<stanly::handle, std::string_view>& handles_to_str;
    result_type
    operator()(auto&& x) const {
      if constexpr (std::same_as<stanly::data, Data>) { return std::format("{}", x); }
      if constexpr (std::same_as<with_handles<stanly::data>, Data>) {
        return std::format("{}", with_handles{x, handles_to_str});
        // return std::format("{}", x);
      }
    };
  };
  auto
  format(const Data& dt, auto& ctx) const {
    if constexpr (std::same_as<stanly::data, Data>) {
      return std::format_to(ctx.out(), "({} {})", dt.template get<0>(), dt.template get<1>());
    }
    if constexpr (std::same_as<with_handles<stanly::data>, Data>) {
      return std::format_to(
          ctx.out(), "({} {})", with_handles{dt.t.template get<0>(), dt.handles_to_str},
          with_handles{dt.t.template get<1>(), dt.handles_to_str});
    }
  }
};

template <class Object, class CharT>
  requires std::same_as<stanly::object, Object> ||
           std::same_as<with_handles<stanly::object>, Object>
struct std::formatter<Object, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const Object& obj, auto& ctx) const {
    if constexpr (std::same_as<stanly::object, Object>) {
      return std::format_to(ctx.out(), "({} {})", obj.template get<0>(), obj.template get<1>());
    }
    if constexpr (std::same_as<with_handles<stanly::object>, Object>) {
      return std::format_to(
          ctx.out(), "({} {})", with_handles{obj.t.template get<0>(), obj.handles_to_str},
          with_handles{obj.t.template get<1>(), obj.handles_to_str});
    }
  }
};

namespace {
template <class Derived, class CharT>
struct scope_or_memory_formatter
    : stanly::lines_arg_parser<std::formatter<std::string_view, CharT>> {
  auto
  format(const auto& x, auto& ctx) const {
    if (ref_to_t(x).is_top()) { return std::format_to(ctx.out(), Derived::s_fmt, Derived::s_top); }
    if (ref_to_t(x).is_bottom()) {
      return std::format_to(ctx.out(), Derived::s_fmt, Derived::s_bot);
    }
    if (this->lines_arg) {
      return std::format_to(ctx.out(), Derived::s_lines_fmt, format_bindings(x));
    }
    return std::format_to(ctx.out(), Derived::s_fmt, format_bindings(x));
  }
};
}  // namespace

template <class Memory, class CharT>
  requires std::same_as<stanly::memory, Memory> ||
           std::same_as<with_handles<stanly::memory>, Memory>
struct std::formatter<Memory, CharT>
    : scope_or_memory_formatter<std::formatter<Memory, CharT>, CharT> {
  constexpr const static char* s_top = "{∀ addr. addr: object(top)}";
  constexpr const static char* s_bot = "{∀ addr. addr: unused}";
  constexpr static const char* s_fmt = "memory{}";
  constexpr static const char* s_lines_fmt = "memory{:lines}";
};

template <class Scope, class CharT>
  requires std::same_as<stanly::scope, Scope> || std::same_as<with_handles<stanly::scope>, Scope>
struct std::formatter<Scope, CharT>
    : scope_or_memory_formatter<std::formatter<Scope, CharT>, CharT> {
  constexpr const static char* s_top = "{∀ var. var: addresses(top)}";
  constexpr const static char* s_bot = "{invalid}";
  constexpr static const char* s_fmt = "scope{}";
  constexpr static const char* s_lines_fmt = "scope{:lines}";
};

inline std::tuple<const stanly::scope&, const stanly::memory&>
get_scope_and_memory(const stanly::state& state) {
  return {
      state.get<stanly::state::idx<stanly::scope>>(),
      state.get<stanly::state::idx<stanly::memory>>()};
}

inline std::tuple<with_handles<stanly::scope>, with_handles<stanly::memory>>
get_scope_and_memory(with_handles<stanly::state> wstate) {
  const auto [scp, mem] = get_scope_and_memory(wstate.t);
  return {
      with_handles<stanly::scope>{scp, wstate.handles_to_str},
      with_handles<stanly::memory>{mem, wstate.handles_to_str}};
}

template <class State, class CharT>
  requires std::same_as<stanly::state, State> || std::same_as<with_handles<stanly::state>, State>
struct std::formatter<State, CharT>
    : stanly::lines_arg_parser<std::formatter<std::string_view, CharT>> {
  auto
  format(const State& state, auto& ctx) const {
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

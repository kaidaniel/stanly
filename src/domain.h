#pragma once

#include <cstddef>
#include <format>
#include <map>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>

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

namespace {
template <class... Args>
inline constexpr bool fail_if = std::false_type::value;

template <class type, class Tuple, std::size_t i = 0>
inline constexpr std::size_t idx = []() {
  if constexpr (std::same_as<type, std::tuple_element_t<i, Tuple>>) {  // NOLINT
    return i;
  } else if constexpr (std::tuple_size_v<Tuple> > i + 1) {
    return idx<type, Tuple, i + 1>;
  } else {
    struct not_in {};
    static_assert(fail_if<type, not_in, Tuple>, "type not in tuple");
    return -1;
  }
}();
enum class RowVarEls { Closed, Open };
using row_var_l = const sparta::BitVectorLattice<RowVarEls, 2>;
const row_var_l l_({RowVarEls::Closed, RowVarEls::Open}, {{RowVarEls::Closed, RowVarEls::Open}});
template <class Derived, class... Args>
struct product : sparta::DirectProductAbstractDomain<Derived, Args...> {
  using dp = sparta::DirectProductAbstractDomain<Derived, Args...>;
  using dp::DirectProductAbstractDomain;
  using tpl = std::tuple<Args...>;
  template <class T>
  [[nodiscard]] const T&
  get() const {
    return dp::template get<idx<T, tpl>>();
  }
  template <class F, std::size_t i = 0>
  void
  operator()(F&& f) {
    if constexpr (requires(std::tuple_element_t<i, tpl>* arg) { f(arg); }) {
      return dp::template apply<stanly::idx<std::tuple_element_t<i, tpl>, tpl>>(std::forward<F>(f));
    } else {
      return this->operator()<F, i + 1>(std::forward<F>(f));
    }
  }
};

}  // namespace

using row_var = sparta::FiniteAbstractDomain<RowVarEls, row_var_l, row_var_l::Encoding, &l_>;
using addresses = sparta::HashedSetAbstractDomain<handle>;
using constant = sparta::ConstantAbstractDomain<handle>;
using defined = sparta::HashedAbstractPartition<handle, addresses>;
using used = sparta::HashedSetAbstractDomain<handle>;

struct record final : product<record, row_var, defined, used> {
  using product::product;
};
struct data final : product<data, record, constant> {
  using product::product;
};
using type = sparta::ConstantAbstractDomain<handle>;

struct object final : product<object, type, data> {
  using product::product;
};

using scope = sparta::HashedAbstractEnvironment<handle, addresses>;
using memory = sparta::HashedAbstractPartition<handle, object>;

struct state final : product<state, scope, memory> {
  using product::product;
};

static_assert(std::derived_from<state, sparta::AbstractDomain<state>>);
static_assert(std::derived_from<scope, sparta::AbstractDomain<scope>>);
static_assert(std::derived_from<memory, sparta::AbstractDomain<memory>>);
}  // namespace stanly

template <class T>
struct with_handles {
  const T& t;
  const std::map<stanly::handle, std::string_view>& handles_to_str =
      stanly::global_string_index.handles();
};

namespace {
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
}  // namespace

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
        (record.template get<row_var>().element() == RowVarEls::Open) ? "* " : "",
        record.template get<defined>().bindings(), record.template get<used>());
  }
};

template <class CharT>
struct std::formatter<with_handles<stanly::record>, CharT>
    : std::formatter<std::string_view, CharT> {
  auto
  format(const with_handles<stanly::record>& record, auto& ctx) const {
    using namespace stanly;
    std::string s_used =
        std::format("{}", with_handles<used>{record.t.template get<used>(), record.handles_to_str});
    return std::format_to(
        ctx.out(), "({}defined{}, used{})",
        (record.t.template get<row_var>().element() == RowVarEls::Open) ? "* " : "",
        format_bindings(with_handles{record.t.template get<defined>(), record.handles_to_str}),
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
      return std::format_to(
          ctx.out(), "({} {})", dt.template get<stanly::record>(),
          dt.template get<stanly::constant>());
    }
    if constexpr (std::same_as<with_handles<stanly::data>, Data>) {
      return std::format_to(
          ctx.out(), "({} {})",
          with_handles{dt.t.template get<stanly::record>(), dt.handles_to_str},
          with_handles{dt.t.template get<stanly::constant>(), dt.handles_to_str});
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
      return std::format_to(
          ctx.out(), "({} {})", obj.template get<stanly::type>(), obj.template get<stanly::data>());
    }
    if constexpr (std::same_as<with_handles<stanly::object>, Object>) {
      return std::format_to(
          ctx.out(), "({} {})",
          with_handles{obj.t.template get<stanly::type>(), obj.handles_to_str},
          with_handles{obj.t.template get<stanly::data>(), obj.handles_to_str});
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
  return {state.get<stanly::scope>(), state.get<stanly::memory>()};
}

inline std::tuple<with_handles<stanly::scope>, with_handles<stanly::memory>>
get_scope_and_memory(with_handles<stanly::state> wstate) {
  return {
      with_handles<stanly::scope>{wstate.t.get<stanly::scope>(), wstate.handles_to_str},
      with_handles<stanly::memory>{wstate.t.get<stanly::memory>(), wstate.handles_to_str}};
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

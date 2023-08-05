#pragma once

#include <cstddef>
#include <format>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>

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
using used = sparta::HashedSetAbstractDomain<handle>;

struct type final
    : sparta::AbstractDomainScaffolding<sparta::acd_impl::ConstantAbstractValue<handle>, type> {
  explicit type(handle h) { this->set_to_value(sparta::acd_impl::ConstantAbstractValue(h)); }
  using AbstractDomainScaffolding::AbstractDomainScaffolding;
  [[nodiscard]] const stanly::handle&
  get() const {
    return this->get_value()->get_constant();
  }
  friend constexpr std::ostream&
  operator<<(std::ostream& os, const stanly::type& h) {
    return os << h.get();
  };
};

struct pointer final : product<pointer, addresses, constant> {
  using product::product;
};
using defined = sparta::HashedAbstractPartition<handle, addresses>;

struct object final : product<object, type, constant, row_var, defined, used> {
  using product::product;
};
using memory = sparta::HashedAbstractPartition<handle, object>;

[[nodiscard]] addresses inline dereference(const pointer& p, const memory& m) {
  const auto& pointer_base = p.get<addresses>();
  if (!p.get<constant>().is_value() || !pointer_base.is_value()) { return pointer_base; }
  auto pointer_field_offset = *p.get<constant>().get_constant();
  addresses out{};
  for (const handle h : pointer_base.elements()) {
    out.join_with(m.get(h).get<defined>().get(pointer_field_offset));
  }
  return out;
}
using scope = sparta::HashedAbstractEnvironment<handle, pointer>;

struct state final : product<state, scope, memory> {
  using product<state, scope, memory>::product;
};

template <class T, class CC>
concept continues = requires(T t, CC cc) { fix_eval_cc(cc, t.eval, t.args); };
template <class T, class Cont, class Result>
concept continues_as = continues<T, Cont> && requires(T t, Cont cc) {
  { fix_eval_cc(cc, t.eval, t.args) } -> std::same_as<Result>;
};
template <class T>
concept abstract_domain = std::derived_from<T, sparta::AbstractDomain<T>>;
template <class CC>
struct nothing {
  std::tuple<> args;
  static void
  eval(CC) {}
};

template <class T, class CC>
auto
eval_cc(CC cc, const T& t) {
  if constexpr (continues<T, CC>) {
    if constexpr (requires {
                    { fix_eval_cc(cc, t.eval, t.args) } -> std::same_as<void>;
                  }) {
      fix_eval_cc(cc, t.eval, t.args);
      return nothing<CC>();
    } else {
      return fix_eval_cc(cc, t.eval, t.args);
    }
  } else {
    return t;
  }
}
template <class CC, class... Args>
auto
fix_eval_cc(CC cc, auto eval, const std::tuple<const Args&...>& tpl) {
  return std::apply([&](auto&&... args) { return eval(cc, eval_cc(cc, args)...); }, tpl);
}

template <class T>
concept object_kind =
    abstract_domain<T> && arg_of<T, object::product> && (!std::same_as<T, object>);

template <class T, class CC = state*>
struct lift {
  std::tuple<const T&> args;
  static const T&
  eval(CC, const T& x) {
    return x;
  }
};
// template<auto F, class... Args>
// struct suspended{
//   std::tuple<const Args&...> args;
//   decltype(F) eval = F;
// };
// template<class T, class CC=memory*>
// struct lift : suspended<[](CC, const T& x){return x;}, T>{};

static_assert(continues<nothing<state*>, state*>);
static_assert(continues<lift<defined>, state*>);
static_assert(continues<lift<used>, state*>);
static_assert(continues<lift<addresses>, state*>);
static_assert(continues_as<lift<addresses>, state*, addresses>);
static_assert(!continues_as<lift<addresses>, state*, defined>);
static_assert(!continues_as<lift<addresses>, state*, void>);

template <class T, class CC = state*>
  requires continues<T, CC>
using eval_type = std::decay_t<decltype(eval_cc(std::declval<CC>(), std::declval<T>()))>;

template <continues_as<state*, addresses> AddressesCont, continues<state*> ObjectKindCont>
  requires object_kind<eval_type<ObjectKindCont>>
struct memory_update {
  std::tuple<const AddressesCont&, const ObjectKindCont&> args;
  static void
  eval(state* s, const addresses& lhs, const eval_type<ObjectKindCont>& rhs) {
    (*s)([&](memory* m) { eval_memory(m, lhs, rhs); });
  }
  static void
  eval_memory(memory* m, const addresses& lhs, const eval_type<ObjectKindCont>& rhs) {
    auto update = [&](auto&& f) {
      return [&](handle h) { m->update(h, [&](object* o) { (*o)(f); }); };
    };
    auto weak_update = update([&](eval_type<ObjectKindCont>* x) { x->join_with(rhs); });
    auto strong_update = update([&](eval_type<ObjectKindCont>* x) { *x = rhs; });

    if (m->is_top() || lhs.is_bottom()) { return; }
    if (lhs.is_top()) {
      for (const auto& [h, _] : m->bindings()) { weak_update(h); };
      return;
    }
    if (lhs.size() == 1) {
      strong_update(*lhs.elements().begin());
      return;
    }
    for (const handle h : lhs.elements()) { weak_update(h); }
  }
};

static_assert(continues<memory_update<lift<addresses>, lift<defined>>, state*>);
static_assert(continues_as<memory_update<lift<addresses>, lift<defined>>, state*, void>);
static_assert(!continues_as<memory_update<lift<addresses>, lift<defined>>, state*, defined>);

template <object_kind T>
memory_update<lift<addresses>, lift<T>>
operator*=(const lift<addresses>& addr, const lift<T>& t) {
  return {{addr, t}};
}

template <continues<state*> Ahead, continues<state*> Behind>
struct sequenced {
  std::tuple<const Ahead&, const Behind&> args;
  static const eval_type<Behind>&
  eval(state*, const eval_type<Ahead>&, const eval_type<Behind>& behind) {
    return behind;
  }
};
template <continues<state*> Ahead, continues<state*> Behind>
sequenced<Ahead, Behind>
operator,(const Ahead& ahead, const Behind& behind) {
  return {{ahead, behind}};
}

template <continues<state*> Ahead, continues<state*> Behind>
sequenced<Ahead, Behind>
operator|(const Ahead& ahead, const Behind& behind) {
  return {{ahead, behind}};
}

static_assert(requires(state* s, sequenced<lift<addresses>, lift<defined>> seq) {
  eval_cc(s, seq);
});

struct object_cc;
template <object_kind T>
struct object_subset {
  std::tuple<const object_cc&> args;
  static const T&
  eval(state*, const object& obj) {
    return obj.get<T>();
  }
};
struct object_cc {
  std::tuple<const handle&> args;
  static const object&
  eval(state* s, const handle& h) {
    return s->get<memory>().get(h);
  }
  template <object_kind T>
  [[nodiscard]] object_subset<T>
  whenever() const {
    return {*this};
  }
};

struct memory_proxy {
  object_cc
  operator[](handle h) const {
    return {h};
  }
} const mem;

struct pointer_cc {
  std::tuple<const handle&> args;
  static const pointer&
  eval(state* s, const handle& h) {
    return s->get<scope>().get(h);
  }
};
struct scope_proxy {
  pointer_cc
  operator[](handle h) const {
    return {h};
  }
} const scp;

template <continues_as<state*, pointer> PointerCont, continues<state*> ObjectKindCont>
  requires object_kind<eval_type<ObjectKindCont>>
struct value_update {
  std::tuple<const PointerCont&, const ObjectKindCont&> args;
  static void
  eval(state* s, const pointer& lhs, const eval_type<ObjectKindCont>& rhs) {
    (*s)([&](memory* m) { eval_memory(m, lhs, rhs); });
  }
  static void
  eval_memory(memory* m, const pointer& lhs, const eval_type<ObjectKindCont>& rhs) {
    auto update = [&](auto&& f) {
      return [&](handle h) { m->update(h, [&](object* o) { (*o)(f); }); };
    };
    auto weak_update = update([&](eval_type<ObjectKindCont>* x) { x->join_with(rhs); });
    auto strong_update = update([&](eval_type<ObjectKindCont>* x) { *x = rhs; });

    if (m->is_top() || lhs.get<addresses>().is_bottom()) { return; }
    auto location = dereference(lhs, *m);
    if (location.is_top()) {
      for (const auto& [h, _] : m->bindings()) { weak_update(h); };
      return;
    }
    if (location.size() == 1) {
      strong_update(*location.elements().begin());
      return;
    }
    for (const handle h : location.elements()) { weak_update(h); }
  }
};

// template<continues_as<state*, pointer> Lhs, continues_as<state*, pointer> Rhs>
// struct reference_update {
//   std::tuple<const Lhs&, const Rhs&> args;
//   static void eval(state* s, const pointer& lhs, const pointer& rhs){
//     const auto& pointer_offset = lhs.get<constant>();
//     if(pointer_offset.is_value()) {
//       (*s)([&](memory* m){update_offset(m, lhs, rhs, *pointer_offset.get_constant());});
//     }
//   }
//   static void update_offset(memory* m, const pointer& lhs, const pointer& rhs, handle
//   pointer_offset){
//     auto update = [&](auto&& f){ return [&](handle h) { m->update(h, [&](object* o){ (*o)(f); });
//     }; }; auto weak_update = update([&](defined* x){ x->join(rhs);}); auto strong_update =
//     update([&](pointer* x){ *x=rhs; });

//     if (m->is_top() || lhs.get<addresses>().is_bottom()) { return; }
//     auto location = lhs.dereference(*m);
//     if (location.is_top()) { for(const auto& [h, _] : m->bindings()){ weak_update(h); }; return;
//     } if (location.size() == 1) { strong_update(*location.elements().begin()); return; }
//     for(const handle h : location.elements()){ weak_update(h); }
//   }
// };

template <object_kind T>
value_update<pointer_cc, lift<T>>
operator*=(const pointer_cc& ptr, const lift<T>& t) {
  return {{ptr, t}};
}
template <object_kind T>
value_update<pointer_cc, object_subset<T>>
operator*=(const pointer_cc& ptr, const object_subset<T>& t) {
  return {{ptr, t}};
}

auto inline asd(state* s, handle h) {
  auto x = [&]() {
    return scp[h] *= mem[h].whenever<defined>(), scp[h] *= mem[h].whenever<constant>();
  };
  eval_cc(s, x());
  return x();
}

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

template <class Type, class CharT>
  requires std::same_as<stanly::type, Type> || std::same_as<with_handles<stanly::type>, Type>
struct std::formatter<Type, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const Type& cnst, auto& ctx) const {
    using namespace stanly;
    std::ostringstream oss{};
    if constexpr (std::same_as<type, Type>) { oss << cnst; }
    if constexpr (std::same_as<with_handles<type>, Type>) {
      switch (cnst.t.kind()) {
        case sparta::AbstractValueKind::Top: {
          [[fallthrough]];
        }
        case sparta::AbstractValueKind::Bottom: {
          oss << cnst.t;
          break;
        }
        case sparta::AbstractValueKind::Value: {
          oss << cnst.handles_to_str.at(cnst.t.get());
          break;
        }
      }
    }
    return std::format_to(ctx.out(), "{}", oss.str());
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
          ctx.out(), "({} {} {} defined{} used{})", obj.template get<stanly::type>(),
          obj.template get<stanly::constant>(),
          obj.template get<stanly::row_var>().element() == stanly::RowVarEls::Open ? "* " : "",
          obj.template get<stanly::defined>().bindings(), obj.template get<stanly::used>());
    }
    if constexpr (std::same_as<with_handles<stanly::object>, Object>) {
      return std::format_to(
          ctx.out(), "({} {} {} defined{} used{})",
          with_handles{obj.t.template get<stanly::type>(), obj.handles_to_str},
          with_handles{obj.t.template get<stanly::constant>(), obj.handles_to_str},
          (obj.t.template get<stanly::row_var>().element() == stanly::RowVarEls::Open) ? "* " : "",
          format_bindings(with_handles{obj.t.template get<stanly::defined>(), obj.handles_to_str}),
          with_handles{obj.t.template get<stanly::used>(), obj.handles_to_str});
    }
  }
};

template <class Pointer, class CharT>
  requires std::same_as<stanly::pointer, Pointer> ||
           std::same_as<with_handles<stanly::pointer>, Pointer>
struct std::formatter<Pointer, CharT> : std::formatter<std::string_view, CharT> {
  auto
  format(const Pointer& obj, auto& ctx) const {
    if constexpr (std::same_as<stanly::pointer, Pointer>) {
      return std::format_to(
          ctx.out(), "({} {})", obj.template get<stanly::addresses>(),
          obj.template get<stanly::constant>());
    }
    if constexpr (std::same_as<with_handles<stanly::pointer>, Pointer>) {
      return std::format_to(
          ctx.out(), "({} {})",
          with_handles{obj.t.template get<stanly::addresses>(), obj.handles_to_str},
          with_handles{obj.t.template get<stanly::constant>(), obj.handles_to_str});
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

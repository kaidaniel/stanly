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
using defined = sparta::HashedAbstractPartition<handle, addresses>;
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

struct object final : product<object, type, constant, row_var, defined, used> {
  using product::product;
};



using memory = sparta::HashedAbstractPartition<handle, object>;
struct pointer final : product<pointer, addresses, constant> {
  using product::product;
  [[nodiscard]] addresses
  dereference(const memory& m) const {
    const auto& pointer_base = get<addresses>();
    if (!get<constant>().is_value() || !pointer_base.is_value()) { return pointer_base; }
    auto pointer_field_offset = *get<constant>().get_constant();
    addresses out{};
    for (const handle h : pointer_base.elements()) {
      out.join_with(m.get(h).get<defined>().get(pointer_field_offset));
    }
    return out;
  }
};
using scope = sparta::HashedAbstractEnvironment<handle, pointer>;


template<class T, class CC>
concept continues = requires(T t, CC cc){ fix_eval_cc(cc, t.eval, t.args);};
template<class T, class Cont, class Result>
concept continues_as = continues<T, Cont> && requires(T t, Cont cc){ { fix_eval_cc(cc, t.eval, t.args) } -> std::same_as<Result>;};
template<class T>
concept abstract_domain = std::derived_from<T, sparta::AbstractDomain<T>>;
struct nothing{
  std::tuple<> args;
  static void eval(memory*){ }
};

template<class T, class CC>
auto eval_cc(CC cc, const T& t){ 
  if constexpr(continues<T, CC>){
    if constexpr(requires{ {fix_eval_cc(cc, t.eval, t.args)} -> std::same_as<void>;} ){
      fix_eval_cc(cc, t.eval, t.args);
      return nothing();
    } else { return fix_eval_cc(cc, t.eval, t.args); }
  } else { return t;}
}
template<class CC, class... Args>
auto fix_eval_cc(CC cc, auto eval, const std::tuple<const Args&...>& tpl){
  return std::apply([&](auto&&... args){ return eval(cc, eval_cc(cc, args)...);}, tpl);
}


template<class T>
concept object_kind = abstract_domain<T> && arg_of<T, object::product> && (!std::same_as<T, object>);

template<class T, class CC=memory*>
struct lift {
  std::tuple<const T&> args;
  static auto eval(CC, const T& x){return x;} 
};
// template<auto F, class... Args>
// struct suspended{
//   std::tuple<const Args&...> args;
//   decltype(F) eval = F;
// };
// template<class T, class CC=memory*>
// struct lift : suspended<[](CC, const T& x){return x;}, T>{};

static_assert(continues<nothing, memory*>);
static_assert(continues<lift<defined>, memory*>);
static_assert(continues<lift<used>, memory*>);
static_assert(continues<lift<addresses>, memory*>);
static_assert(continues_as<lift<addresses>, memory*, addresses>);
static_assert(!continues_as<lift<addresses>, memory*, defined>);
static_assert(!continues_as<lift<addresses>, memory*, void>);

template<class T, class CC=memory*>
requires continues<T, CC>
using eval_type = std::decay_t<decltype(eval_cc(std::declval<CC>(), std::declval<T>()))>;

template<continues_as<memory*, addresses> AddressesCont, continues<memory*> ObjectKindCont>
requires object_kind<eval_type<ObjectKindCont>>
struct memory_update {
  std::tuple<const AddressesCont&, const ObjectKindCont&> args;
  static void eval(memory* m, const addresses& lhs, const eval_type<ObjectKindCont>& rhs){ 
    auto update = [&](auto&& f){ return [&](handle h) { m->update(h, [&](object* o){ (*o)(f); }); }; };
    auto weak_update = update([&](eval_type<ObjectKindCont>* x){ x->join_with(rhs); });
    auto strong_update = update([&](eval_type<ObjectKindCont>* x){ *x=rhs; });
    if (m->is_top() || lhs.is_bottom()) { return; }
    if (lhs.is_top()) { for(const auto& [h, _] : m->bindings()){ weak_update(h); }; return; }
    if (lhs.size() == 1) { strong_update(*lhs.elements().begin()); return; }
    for(const handle h : lhs.elements()){ weak_update(h); }
  }
};



static_assert(continues<memory_update<lift<addresses>, lift<defined>>, memory*>);
static_assert(continues_as<memory_update<lift<addresses>, lift<defined>>, memory*, void>);
static_assert(!continues_as<memory_update<lift<addresses>, lift<defined>>, memory*, defined>);

template<object_kind T>
memory_update<lift<addresses>, lift<T>> operator*=(const lift<addresses>& addr, const lift<T>& t){ return {{addr, t}};}


template<continues<memory*> Ahead, continues<memory*> Behind>
struct sequenced{
  std::tuple<const Ahead&, const Behind&> args;
  static void eval(memory*, const eval_type<Ahead>&, const eval_type<Behind>& behind){ return behind; }
};
template<continues<memory*> Ahead, continues<memory*> Behind>
sequenced<Ahead, Behind> operator,(const Ahead& ahead, const Behind& behind) { return {{ahead, behind}}; }

void inline asd(lift<addresses> a, lift<defined> d){ a*=d, a*=d;}

struct object_reference {
  std::tuple<const handle&> args;
  static lift<object> eval(memory* m, const handle& h) { return {m->get(h)}; }
};

struct memory_proxy{
  object_reference operator[](handle h) { return {h};}
} const mem;


struct state final : product<state, scope, memory> {
  using product<state, scope, memory>::product;
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

#include <string_view>
#include <variant>
#include <vector>

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "firstorder-syntax.h"

namespace stanly {

template <typename D>
concept abstract_domain =
    std::derived_from<typename D::domain, sparta::AbstractDomain<typename D::domain>> &&
    requires { typename D::domain; };

namespace firstorder {

using Kind = sparta::AbstractValueKind;
/// Abstraction of the program state (Var -> Value).
template <class Domain>
struct domain_mixin : sparta::AbstractDomain<Domain> {
  using domain = Domain;
  template <class... Args>
  domain_mixin(Args&&... args) : domain_(std::forward<Args>(args)...) {}

 private:
  domain domain_{};
};
template <class Repr>
struct domain {
  using str_mixin = domain_mixin<sparta::ConstantAbstractDomain<Repr>>;
  using record_mixin = domain_mixin<sparta::HashedSetAbstractDomain<Repr>>;
  template <class S, class O>
  using env_mixin = domain_mixin<sparta::HashedAbstractEnvironment<S, O>>;
  template <class O, class S, class R>
  using object_mixin = sparta::DirectProductAbstractDomain<O, S, R>;
  struct str : str_mixin {
    using str_mixin::domain_mixin;
  };
  struct record : record_mixin {
    using record_mixin::domain_mixin;
  };
  struct object : object_mixin<object, str, record> {
    using object_mixin<object, str, record>::object_mixin;
  };

  struct env : env_mixin<str, object> {
    using env_mixin<str, object>::env_mixin;
  };
  using types = std::tuple<str, record, object, env>;
  struct top {};
  struct bottom {};
  using recordv = record;
};
auto const analyse = [](std::vector<firstorder::syntax<idx>::node>) -> domain<std::size_t>::env {
  return {};
};

// static_assert(abstract_domain<domain<idx>>);
}  // namespace firstorder
}  // namespace stanly
// TODO: replace with actual abstract domain in src/firstorder-syntax.h
// template<class Repr>
// struct domain {
//   struct str { Repr val; };
//   struct record { Repr field1; Repr field2; };
//   struct recordv { Repr field1; Repr field2; };
//   struct top{};
//   struct bottom{};
//   using object = std::variant<str, record, recordv, top, bottom>;
//   using env = std::unordered_map<Repr, object>;
//  };

namespace stanly {
template <class T>
  requires contains<firstorder::domain<std::size_t>::object, std::decay_t<T>>
struct is_syntax_node<T> {
  constexpr static bool value = true;
};
}  // namespace stanly

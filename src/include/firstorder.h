#ifndef KSAR_FIRST_ORDER_H
#define KSAR_FIRST_ORDER_H

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include <functional>

namespace ksar {
namespace syntax {
  using Var = int;
  using Literal = int;
  // clang-format off
/// @brief declare variable ".var" to be an input to the program.
struct Input { Var var; };
/// @brief declare ".var" to be a local variable.
struct Local { Var var; };
/// @brief .target[.stored_field] = .rhs
struct StoreSubscript { Var rhs; Var target; Var subscript; };
/// @brief .lhs = .source[.subscript]
struct LoadSubscript { Var lhs; Var source; Var subscript; };
/// @brief .lhs = .literal
struct AssignLiteral { Var lhs; Literal literal; };
/// @brief .lhs = .rhs
struct AssignVar { Var lhs; Var rhs; };
  // clang-format on
} // namespace syntax

namespace domain {
  /// @brief Abstraction of a const-propagated literal.
  using Element = sparta::ConstantAbstractDomain<syntax::Literal>;
  /// @brief Abstraction of a dynamic record as a set of field names.
  using Record = sparta::HashedSetAbstractDomain<syntax::Literal>;
  /// @brief Abstraction of a set of values in memory (elements or objects).
  struct Value
      : public sparta::DirectProductAbstractDomain<Value, Element, Record> {
    using Product = sparta::DirectProductAbstractDomain<Value, Element, Record>;
    using Product::DirectProductAbstractDomain;
  };
  /// @brief Abstraction of the program state (Var -> Value).
  using Bindings = sparta::HashedAbstractEnvironment<syntax::Var, Value>;
  static const Element &element(const Bindings &b, const syntax::Var &v) {
    return b.get(v).get<0>();
  }
  static const Record &record(const Bindings &b, const syntax::Var &v) {
    return b.get(v).get<1>();
  }
  template <typename F, typename G>
  static void update(F f, G g, Bindings &b, const syntax::Var &v) {
    b.update(v, [&](Value *value) {
      value->apply<0>([&](Element *element) { std::invoke(f, element); });
      value->apply<1>([&](Record *record) { std::invoke(g, record); });
    });
  }
} // namespace domain

void transition(const syntax::Input &, domain::Bindings &);
void transition(const syntax::Local &, domain::Bindings &);
void transition(const syntax::StoreSubscript &, domain::Bindings &);
void transition(const syntax::LoadSubscript &, domain::Bindings &);
void transition(const syntax::AssignLiteral &, domain::Bindings &);
void transition(const syntax::AssignVar &, domain::Bindings &);
} // namespace ksar

#endif // KSAR_FIRST_ORDER_H

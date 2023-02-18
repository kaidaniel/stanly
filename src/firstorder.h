#ifndef KSAR_FIRST_ORDER_H
#define KSAR_FIRST_ORDER_H

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include <variant>

namespace ksar {
using Var = int;
using Literal = int;
using Fields = std::vector<Literal>;
/// Abstraction of a const-propagated literal.
using Element = sparta::ConstantAbstractDomain<Literal>;
/// Abstraction of a dynamic record as a set of field names.
using Record = sparta::HashedSetAbstractDomain<Literal>;
/// Abstraction of a set of values in memory (elements or objects).
struct Value
    : public sparta::DirectProductAbstractDomain<Value, Element, Record> {
  using Product = sparta::DirectProductAbstractDomain<Value, Element, Record>;
  using Product::DirectProductAbstractDomain;
};
/// Abstraction of the program state (Var -> Value).
using Bindings = sparta::HashedAbstractEnvironment<Var, Value>;
// clang-format off
/// declare variable ".var" to be an input to the program.
struct Input { Var var; };
/// declare ".var" to be a local variable.
struct Local { Var var; };
/// .target[.stored_field] = .rhs
struct StoreSubscript { Var rhs; Var target; Var subscript; };
/// .lhs = .source[.subscript]
struct LoadSubscript { Var lhs; Var source; Var subscript; };
/// .lhs = .literal
struct AssignLiteral { Var lhs; Literal literal; };
/// .lhs = .record
struct AllocRecord { Var lhs; Fields fields; };
/// .lhs = .rhs
struct AssignVar { Var lhs; Var rhs; };
  // clang-format on
using FirstOrderSyntax = std::variant<Input, Local, StoreSubscript, LoadSubscript, AssignLiteral, AllocRecord, AssignVar>;

void transition(const Input &, Bindings &);
void transition(const Local &, Bindings &);
void transition(const StoreSubscript &, Bindings &);
void transition(const LoadSubscript &, Bindings &);
void transition(const AssignLiteral &, Bindings &);
void transition(const AssignVar &, Bindings &);
void transition(const AllocRecord&, Bindings &);
} // namespace ksar

#endif // KSAR_FIRST_ORDER_H

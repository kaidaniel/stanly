#ifndef KSAR_FIRST_ORDER_H
#define KSAR_FIRST_ORDER_H

#include <functional>
#include <memory>
#include <string>
#include <variant>

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"

namespace syntax {
struct Input;
struct Local;
struct StoreSubscript;
struct LoadSubscript;
struct AssignScalar;
struct AssignVar;
} // namespace syntax

namespace concrete {
using Var = int;
using Scalar = int;
using Object = std::unordered_map<concrete::Scalar, concrete::Scalar>;
using Value = std::variant<concrete::Scalar, concrete::Object>;
using Bindings = std::unordered_map<Var, concrete::Value>;
void transition(const syntax::Input &, concrete::Bindings &);
void transition(const syntax::Local &, concrete::Bindings &);
void transition(const syntax::StoreSubscript &, concrete::Bindings &);
void transition(const syntax::LoadSubscript &, concrete::Bindings &);
void transition(const syntax::AssignScalar &, concrete::Bindings &);
void transition(const syntax::AssignVar &, concrete::Bindings &);
} // namespace concrete

namespace syntax {
/// @brief declare variable ".var" to be an input to the program.
///
/// Inputs start as top (abstracting over all possible values).
struct Input {
  concrete::Var var;
};
/// @brief declare ".var" to be a local variable.
///
/// Local variables start as bottom (uninitialized).
struct Local {
  concrete::Var var;
};
/// @brief .target[.stored_field] = .rhs
struct StoreSubscript {
  concrete::Var rhs;
  concrete::Var target;
  concrete::Var subscript;
};
/// @brief .lhs = .source[.subscript]
struct LoadSubscript {
  concrete::Var lhs;
  concrete::Var source;
  concrete::Var subscript;
};
/// @brief .lhs = .scalar
struct AssignScalar {
  concrete::Var lhs;
  concrete::Scalar scalar;
};
/// @brief .lhs = .rhs
struct AssignVar {
  concrete::Var lhs;
  concrete::Var rhs;
};
} // namespace syntax

namespace abstract {
/// @brief Abstraction of a scalar value.
using Scalar = sparta::ConstantAbstractDomain<concrete::Scalar>;
/// @brief Abstraction of an object as a set of field names.
///
// abs_obj = [1 -> abstract::Value{1}][2 -> Top] means:
// Union(paths where 1 is set to value 1, paths where 2 is set to any value)
//   -> over-approximates e.g. {{1:1, 2:3}, {1:1}}
// if combined with under-approximation of labels present,
//   then: {1,2} means can perform strong update.
// {1} means '2' can perform only weak update.
using Object = sparta::HashedSetAbstractDomain<concrete::Scalar>;
/// @brief Abstraction of a set of values in memory (scalars or objects).
struct Value
    : public sparta::DirectProductAbstractDomain<Value, Scalar, Object> {
  using sparta::DirectProductAbstractDomain<
      Value, Scalar, Object>::DirectProductAbstractDomain;
  static const int ScalarIdx = 0;
  static const int ObjectIdx = 1;
};
/// @brief Abstraction of the program state (Var -> Value).
using Bindings = sparta::HashedAbstractEnvironment<concrete::Var, Value>;
/// @brief Set Input.var to top (no-op).
void transition(const syntax::Input &, abstract::Bindings &);
/// @brief Bind Local.var to bottom.
void transition(const syntax::Local &, abstract::Bindings &);
/// @brief Update the abstract field ".subscript_var" of .object_var to
/// .scalar_var
///
/// If .scalar_var abstracts
void transition(const syntax::StoreSubscript &, abstract::Bindings &);
/// @brief Update .scalar_var to the value of the abstract field
/// ".subscript_var" of .object_var
void transition(const syntax::LoadSubscript &, abstract::Bindings &);
/// @brief Strongly update .scalar_var to .scalar
void transition(const syntax::AssignScalar &, abstract::Bindings &);
/// @brief Update .target_var to .source_var
void transition(const syntax::AssignVar &, abstract::Bindings &);
} // namespace abstract

#endif // KSAR_FIRST_ORDER_H

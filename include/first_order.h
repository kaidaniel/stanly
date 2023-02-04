#ifndef KSAR_FIRST_ORDER_H
#define KSAR_FIRST_ORDER_H

#include <functional>
#include <memory>
#include <string>
#include <variant>

#include "ConstantAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "ReducedProductAbstractDomain.h"

namespace syntax {
struct Input;
struct Local;
struct StoreSubscript;
struct LoadSubscript;
struct AssignScalar;
struct AssignVar;
}  // namespace syntax

namespace concrete {
using Var = int;
using Scalar = int;
using Object = std::vector<concrete::Scalar>;
using Bindings = std::unordered_map<Var, concrete::Scalar>;
using Value = std::variant<concrete::Scalar, concrete::Object>;
void transition(const syntax::Input &, concrete::Bindings &);
void transition(const syntax::Local &, concrete::Bindings &);
void transition(const syntax::StoreSubscript &, concrete::Bindings &);
void transition(const syntax::LoadSubscript &, concrete::Bindings &);
void transition(const syntax::AssignScalar &, concrete::Bindings &);
void transition(const syntax::AssignVar &, concrete::Bindings &);
}  // namespace concrete

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
/// @brief .object_var[.subscript_var] = .scalar_var
struct StoreSubscript {
  concrete::Var scalar;
  concrete::Var object;
  concrete::Var subscript;
};
/// @brief .scalar_var = .object_var[.subscript_var]
struct LoadSubscript {
  concrete::Var scalar;
  concrete::Var object;
  concrete::Var subscript;
};
/// @brief .scalar_var = .scalar
struct AssignScalar {
  concrete::Var var;
  concrete::Scalar scalar;
};
/// @brief .target_var = .source_var
struct AssignVar {
  concrete::Var target;
  concrete::Var source;
};
}  // namespace syntax

namespace abstract {
/// @brief Abstraction of a scalar value.
using Scalar = sparta::ConstantAbstractDomain<concrete::Scalar>;
/// @brief Abstraction of an object as a set of field names.
///
// abs_obj = [1 -> Abs{1}][2 -> Top] means: Union(paths where 1 is set to value
// 1, paths where 2 is set to any value) -> over-approximates e.g. {{1:1, 2:3},
// {1:1}} if combined with under-approximation of labels present, then: {1,2}
// means can perform strong update. {1} means '2' can perform only weak update.
using Object = sparta::HashedSetAbstractDomain<concrete::Scalar>;
/// @brief Abstraction of a set of values in memory (scalars or objects).
struct Value
    : public sparta::ReducedProductAbstractDomain<Value, Scalar, Object> {
  using sparta::ReducedProductAbstractDomain<
      Value, Scalar, Object>::ReducedProductAbstractDomain;
  static void reduce_product(std::tuple<Scalar, Object> &) {}
};
/// @brief Abstraction of the program state (Var -> Value).
class Bindings {
 private:
  void bind(const concrete::Var &, const abstract::Value &);
  using Impl = sparta::HashedAbstractEnvironment<concrete::Var, Value>;
  Impl impl_;

 public:
  Bindings(Impl &&);
  void bind_top(const concrete::Var &);
  void bind_bottom(const concrete::Var &);
};

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
}  // namespace abstract
#endif  // KSAR_FIRST_ORDER_H

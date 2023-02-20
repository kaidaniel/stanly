#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include <vector>
#include <variant>
#include "stanly-api.h"

namespace stanly {
using Var = int;
using NumberLiteral = int;
using RecordLiteral = std::vector<NumberLiteral>;
/// Abstraction of a const-propagated literal.
using Number = sparta::ConstantAbstractDomain<NumberLiteral>;
/// Abstraction of a dynamic record as a set of field names.
using Record = sparta::HashedSetAbstractDomain<NumberLiteral>;
/// Abstraction of a set of values in memory (elements or objects).
struct Value
    : public sparta::DirectProductAbstractDomain<Value, Number, Record> {
  using Product = sparta::DirectProductAbstractDomain<Value, Number, Record>;
  using Product::DirectProductAbstractDomain;
};
/// Abstraction of the program state (Var -> Value).
using Bindings = sparta::HashedAbstractEnvironment<Var, Value>;
// clang-format off
/// declare `var` to be a local variable., e.g. `x=unknown_func()`
struct DeclareLocalVar { Var var; };
/// `target` [ `field` ] = `rhs`, e.g. `x[y]=z`
struct SetField { Var rhs; Var target; Var field; };
/// `lhs` = `source` [ `subscript` ], e.g. `x=y[z]`
struct LoadField { Var lhs; Var source; Var field; };
/// `lhs` = `number`, e.g. `x=3`
struct LoadNumber { Var lhs; NumberLiteral number_literal; };
/// `lhs` = `record`, e.g. `x={1: "a", 2: "b"}`
struct LoadRecord { Var lhs; RecordLiteral record_literal; };
/// `lhs` = `rhs`
struct LoadVar { Var lhs; Var rhs; };
// clang-format on


using FirstOrderSyntax = std::variant<
    DeclareLocalVar, SetField, LoadField, LoadNumber, LoadRecord, LoadVar>;
using Kind = sparta::AbstractValueKind;

class FirstOrderGraph{
    std::vector<FirstOrderSyntax> nodes;
    public:
    template<class... Args>
    void insert(Args&&... args);
    friend std::string show(const FirstOrderGraph&);
};
class FirstOrderAnalysis;
std::string show(const FirstOrderGraph&);
std::string show(const FirstOrderAnalysis&);
Analysis analyse(const FirstOrderGraph&);
}
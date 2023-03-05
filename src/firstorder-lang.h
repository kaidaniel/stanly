#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "metaprogramming.h"
#include <string_view>
#include <vector>

namespace stanly {

using Var = std::string_view;
using TextLiteral = std::string_view;
using RecordLiteral = std::vector<TextLiteral>;
/// Abstraction of a const-propagated literal.
using Text = sparta::ConstantAbstractDomain<TextLiteral>;
/// Abstraction of a dynamic record as a set of field names.
using Record = sparta::HashedSetAbstractDomain<TextLiteral>;
/// Abstraction of a set of values in memory (elements or objects).
struct Value : public sparta::DirectProductAbstractDomain<Value, Text, Record> {
  using Product = sparta::DirectProductAbstractDomain<Value, Text, Record>;
  using Product::DirectProductAbstractDomain;
};
/// Abstraction of the program state (Var -> Value).
using Bindings = sparta::HashedAbstractEnvironment<Var, Value>;
// clang-format off
/// `target` [ `field` ] = `rhs`, e.g. `x[y]=z`
struct SetField { Var rhs; Var target; Var field; };
/// `lhs` = `source` [ `subscript` ], e.g. `x=y[z]`
struct LoadField { Var lhs; Var source; Var field; };
/// `lhs` = `text_literal`, e.g. `x="abc"` or `x=1`
struct LoadText { Var lhs; TextLiteral text_literal; };
/// `lhs` = `record`, e.g. `x={"a": 1, "b": 2}`
struct LoadRecord { Var lhs; RecordLiteral record_literal; };
/// `lhs` = `rhs`
struct LoadVar { Var lhs; Var rhs; };
// clang-format on

using Kind = sparta::AbstractValueKind;
using FirstOderSyntaxNodes = metaprogramming::TypeList<
    SetField, LoadField, LoadText, LoadRecord, LoadVar>;

} // namespace stanly
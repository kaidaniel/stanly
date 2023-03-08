#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "metaprogramming.h"
#include <string_view>
#include <variant>
#include <vector>

namespace stanly {

using Var = std::string_view;
using TextLiteral = std::string_view;
using RecordLiteral = std::vector<TextLiteral>;
using Text = sparta::ConstantAbstractDomain<TextLiteral>;
using Record = sparta::HashedSetAbstractDomain<TextLiteral>;
struct Value : public sparta::DirectProductAbstractDomain<Value, Text, Record> {
  using Product = sparta::DirectProductAbstractDomain<Value, Text, Record>;
  using Product::DirectProductAbstractDomain;
};
/// Abstraction of the program state (Var -> Value).
using Bindings = sparta::HashedAbstractEnvironment<Var, Value>;
// clang-format off
struct SetField { Var rhs; Var target; Var field; };
struct LoadField { Var lhs; Var source; Var field; };
struct LoadText { Var lhs; TextLiteral text_literal; };
struct LoadRecord { Var lhs; RecordLiteral record_literal; };
struct LoadVar { Var lhs; Var rhs; };
struct LoadTop { Var lhs; TextLiteral text_literal;};
// clang-format on

using Kind = sparta::AbstractValueKind;
using FirstOrderSyntaxNode = metaprogramming::TypeList<
    SetField, LoadField, LoadText, LoadRecord, LoadVar, LoadTop>;
void parse_firstorder(
    std::string_view program,
    const std::function<void(
        const metaprogramming::rebind_t<std::variant, FirstOrderSyntaxNode>)>
        &callback);
} // namespace stanly
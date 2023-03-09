#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "iterator.h"
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
template<class Var, class TextLiteral, class RecordLiteral>
union LanguageRep{
struct SetField { Var rhs; Var target; Var field; };
struct LoadField { Var lhs; Var source; Var field; };
struct LoadText { Var lhs; TextLiteral text_literal; };
struct LoadRecord { Var lhs; RecordLiteral record_literal; };
struct LoadVar { Var lhs; Var rhs; };
struct LoadTop { Var lhs; TextLiteral text_literal;};
};
using Language = LanguageRep<Var, TextLiteral, RecordLiteral>;
using SetField = Language::SetField;
using LoadField = Language::LoadField;
using LoadText = Language::LoadText;
using LoadRecord = Language::LoadRecord;
using LoadVar = Language::LoadVar;
using LoadTop = Language::LoadTop;

// clang-format on

using Kind = sparta::AbstractValueKind;
using FirstOrderSyntaxNode = metaprogramming::TypeList<
    SetField, LoadField, LoadText, LoadRecord, LoadVar, LoadTop>;

using Syntax = metaprogramming::rebind_t<std::variant, FirstOrderSyntaxNode>;
iterator::inpt_range<Syntax> parse_firstorder(std::string_view);

} // namespace stanly
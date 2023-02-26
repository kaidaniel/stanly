#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include <vector>
#include <variant>
#include "stanly-api.h"
#include <iostream>

namespace stanly {
using Var = int;
using TextLiteral = std::string;
using RecordLiteral = std::vector<TextLiteral>;
/// Abstraction of a const-propagated literal.
using Text = sparta::ConstantAbstractDomain<TextLiteral>;
/// Abstraction of a dynamic record as a set of field names.
using Record = sparta::HashedSetAbstractDomain<TextLiteral>;
/// Abstraction of a set of values in memory (elements or objects).
struct Value
    : public sparta::DirectProductAbstractDomain<Value, Text, Record> {
  using Product = sparta::DirectProductAbstractDomain<Value, Text, Record>;
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
/// `lhs` = `text_literal`, e.g. `x="abc"`
struct LoadText { Var lhs; TextLiteral text_literal; };
/// `lhs` = `record`, e.g. `x={"a": 1, "b": 2}`
struct LoadRecord { Var lhs; RecordLiteral record_literal; };
/// `lhs` = `rhs`
struct LoadVar { Var lhs; Var rhs; };
// clang-format on


using Kind = sparta::AbstractValueKind;

std::string show(const DeclareLocalVar&);
std::string show(const SetField&);
std::string show(const LoadField&);
std::string show(const LoadText&);
std::string show(const LoadRecord&);
std::string show(const LoadVar&);

struct Moveonly {
  ~Moveonly() = default;
  Moveonly() = default;
  Moveonly(const Moveonly&) = delete;
  Moveonly& operator=(const Moveonly&) = delete;
  Moveonly(Moveonly&&) noexcept {std::cout << "Moveonly(Moveonly&&)\n";};
  Moveonly& operator=(Moveonly&&) noexcept {std::cout << "operator=(Moveonly&&)\n"; return *this;};
};

class FirstOrderSyntax {
  using Variant = std::variant<DeclareLocalVar, SetField, LoadField, LoadText, LoadRecord, LoadVar>;
  Variant variant_;
  public:
  friend std::string show(const FirstOrderSyntax&);
  FirstOrderSyntax(Variant variant) : variant_(std::move(variant)) {}
};

class FirstOrderGraph : public Moveonly {
    std::vector<FirstOrderSyntax> nodes_;
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
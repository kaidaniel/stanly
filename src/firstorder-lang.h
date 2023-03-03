#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "stanly-api.h"
#include <iostream>
#include <variant>
#include <vector>
#include <string_view>
#include <unordered_map>

namespace stanly {
using Var = int;
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
/// declare `var` to be a local variable., e.g. `x=unknown_func()`
struct DeclareLocalVar { Var var; };
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

class FirstOrderGraph {
  using Syntax = std::variant<
      DeclareLocalVar, SetField, LoadField, LoadText, LoadRecord, LoadVar>;
  std::vector<Syntax> nodes_;
  std::string program_;
  class VariablePool {
    std::unordered_map<std::string_view, Var> var_to_idx_{};
    std::vector<std::string_view> idx_to_var_{};
    Var max_{-1};  // start at -1 so the index of the first insert is 0.
    public:
      Var idx(std::string_view);
      std::string_view var(Var) const;
  } variable_pool_;

public:
  template <class... Args> void insert(Args &&...args);
  friend std::string show(const FirstOrderGraph &);
  std::string_view program() { return program_; }
  Var idx(std::string_view);
  std::string_view var(Var) const;
  FirstOrderGraph(std::string program);
};
class FirstOrderAnalysis;
std::string show(const FirstOrderGraph &);
std::string show(const FirstOrderAnalysis &);
Analysis analyse(const FirstOrderGraph &);
} // namespace stanly
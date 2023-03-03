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
#include "fmt/core.h"

#ifndef NDEBUG
#include <exception>
#include "boost/stacktrace.hpp" // std::cout << boost::stacktrace::stacktrace();
#endif


namespace stanly {
using VarIdx = int;
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
using Bindings = sparta::HashedAbstractEnvironment<VarIdx, Value>;
// clang-format off
/// declare `var` to be a local variable., e.g. `x=unknown_func()`
struct DeclareLocalVar { VarIdx var; };
/// `target` [ `field` ] = `rhs`, e.g. `x[y]=z`
struct SetField { VarIdx rhs; VarIdx target; VarIdx field; };
/// `lhs` = `source` [ `subscript` ], e.g. `x=y[z]`
struct LoadField { VarIdx lhs; VarIdx source; VarIdx field; };
/// `lhs` = `text_literal`, e.g. `x="abc"` or `x=1`
struct LoadText { VarIdx lhs; TextLiteral text_literal; };
/// `lhs` = `record`, e.g. `x={"a": 1, "b": 2}`
struct LoadRecord { VarIdx lhs; RecordLiteral record_literal; };
/// `lhs` = `rhs`
struct LoadVar { VarIdx lhs; VarIdx rhs; };
// clang-format on


using Kind = sparta::AbstractValueKind;
class FirstOrderGraph {
  using Syntax = std::variant<
      DeclareLocalVar, SetField, LoadField, LoadText, LoadRecord, LoadVar>;
  std::vector<Syntax> nodes_;
  std::string program_;
  class VariablePool {
    std::unordered_map<std::string_view, VarIdx> var_name_to_idx_{};
    std::vector<std::string_view> var_idx_to_var_{};
    VarIdx max_{-1};  // start at -1 so the index of the first insert is 0.
    public:
      VarIdx var_name_to_idx(std::string_view);
      std::string_view var_idx_to_name(VarIdx) const;
  } variable_pool_;

public:
  template <class... Args> void insert(Args &&...args);
  friend std::string show(const FirstOrderGraph &);
  std::string_view program() { return program_; }
  VarIdx var_name_to_idx(std::string_view);
  std::string_view var_idx_to_name(VarIdx) const;
  FirstOrderGraph(std::string program)
    : program_(std::move(program)) {}
  FirstOrderGraph(const FirstOrderGraph&) {
      #ifndef NDEBUG
      std::cout << boost::stacktrace::stacktrace();
      throw std::logic_error("Copied FirstOrderGraph");
      #endif
  };
  FirstOrderGraph(FirstOrderGraph&&){
      #ifndef NDEBUG
      std::cout << boost::stacktrace::stacktrace();
      throw std::logic_error("Copied FirstOrderGraph");
      #endif
  }
  FirstOrderGraph operator=(FirstOrderGraph&&) = delete;
  FirstOrderGraph operator=(const FirstOrderGraph&) = delete;
};
class FirstOrderAnalysis;
std::string show(const FirstOrderGraph &);
std::string show(const FirstOrderAnalysis &);
Analysis analyse(const FirstOrderGraph &);
} // namespace stanly
#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "analysis.h"
#include <memory>
#include <variant>

namespace ksar {
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

namespace {
  auto number(const Bindings &b, const Var &v) -> const Number & {
    return b.get(v).get<0>();
  }
  auto record(const Bindings &b, const Var &v) -> const Record & {
    return b.get(v).get<1>();
  }

  void update(auto fnumber, auto frecord, Bindings &b, const Var &v) {
    b.update(v, [&](Value *value) {
      value->apply<0>([&](Number *number) { std::invoke(fnumber, number); });
      value->apply<1>([&](Record *record) { std::invoke(frecord, record); });
    });
  }
} // namespace

void transition(const DeclareLocalVar &n, Bindings &b) {
  b.set(n.var, Value::bottom());
}
void transition(const SetField &n, Bindings &b) {
  update(
      &Number::set_to_bottom,
      [&](Record *record) {
        switch (const auto &field{number(b, n.field)}; field.kind()) {
        case Kind::Bottom: record->set_to_bottom(); break;
        case Kind::Top: record->set_to_top(); break;
        case Kind::Value: record->add(*field.get_constant()); break;
        }
      },
      b, n.target);
}
void transition(const LoadField &n, Bindings &b) {
  update(
      (record(b, n.source).is_bottom() or number(b, n.field).is_bottom())
          ? &Number::set_to_bottom
          : &Number::set_to_top,
      &Record::set_to_bottom, b, n.lhs);
}
void transition(const LoadNumber &n, Bindings &b) {
  auto set_to_literal = [&](Number *e) { *e = Number{n.number_literal}; };
  update(set_to_literal, &Record::set_to_bottom, b, n.lhs);
}
void transition(const LoadRecord &n, Bindings &b) {
  auto set_to_record = [&](Record *r) {
    *r = Record{};
    r->add(n.record_literal.begin(), n.record_literal.end());
  };
  update(&Number::set_to_bottom, set_to_record, b, n.lhs);
}
void transition(const LoadVar &n, Bindings &b) { b.set(n.lhs, b.get(n.rhs)); }

class FirstOrderLanguageGraph {};
class FirstOrderLanguageAnalysis {};
std::string show(const FirstOrderLanguageGraph &) {
  return "FirstOrderLanguageGraph";
}
std::string show(const FirstOrderLanguageAnalysis &) {
  return "FirstOrderLanguageAnalysis";
}
Analysis analyse(const FirstOrderLanguageGraph &) {
  return Analysis{FirstOrderLanguageAnalysis{}};
}
Graph parse(const std::string &) { return Graph{FirstOrderLanguageGraph{}}; }
}; // namespace ksar

#include "firstorder-lang.h"

namespace stanly {
namespace {
  auto text(const Bindings &b, const VarIdx &v) -> const Text & {
    return b.get(v).get<0>();
  }
  auto record(const Bindings &b, const VarIdx &v) -> const Record & {
    return b.get(v).get<1>();
  }

  void update(auto fnumber, auto frecord, Bindings &b, const VarIdx &v) {
    b.update(v, [&](Value *value) {
      value->apply<0>([&](Text *text) { std::invoke(fnumber, text); });
      value->apply<1>([&](Record *record) { std::invoke(frecord, record); });
    });
  }
} // namespace

void transition(const DeclareLocalVar &n, Bindings &b) {
  b.set(n.var, Value::bottom());
}
void transition(const SetField &n, Bindings &b) {
  update(
      &Text::set_to_bottom,
      [&](Record *record) {
        switch (const auto &field{text(b, n.field)}; field.kind()) {
        case Kind::Bottom: record->set_to_bottom(); break;
        case Kind::Top: record->set_to_top(); break;
        case Kind::Value: record->add(*field.get_constant()); break;
        }
      },
      b, n.target);
}
void transition(const LoadField &n, Bindings &b) {
  update(
      (record(b, n.source).is_bottom() or text(b, n.field).is_bottom())
          ? &Text::set_to_bottom
          : &Text::set_to_top,
      &Record::set_to_bottom, b, n.lhs);
}
void transition(const LoadText &n, Bindings &b) {
  auto set_to_literal = [&](Text *e) { *e = Text{n.text_literal}; };
  update(set_to_literal, &Record::set_to_bottom, b, n.lhs);
}
void transition(const LoadRecord &n, Bindings &b) {
  auto set_to_record = [&](Record *r) {
    *r = Record{};
    r->add(n.record_literal.begin(), n.record_literal.end());
  };
  update(&Text::set_to_bottom, set_to_record, b, n.lhs);
}
void transition(const LoadVar &n, Bindings &b) { b.set(n.lhs, b.get(n.rhs)); }

class FirstOrderAnalysis {};
std::string show(const FirstOrderAnalysis &) { return "FirstOrderAnalysis"; }
Analysis analyse(const FirstOrderGraph &) {
  return Analysis{FirstOrderAnalysis{}};
}
} // namespace stanly
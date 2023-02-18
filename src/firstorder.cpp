#include "firstorder.h"

namespace ksar {
using Kind = sparta::AbstractValueKind;

namespace {
  auto element(const Bindings &b, const Var &v) -> const Element & {
    return b.get(v).get<0>();
  }
  auto record(const Bindings &b, const Var &v) -> const Record & {
    return b.get(v).get<1>();
  }

  void update(auto felement, auto frecord, Bindings &b, const Var &v) {
    b.update(v, [&](Value *value) {
      value->apply<0>(
          [&](Element *element) { std::invoke(felement, element); });
      value->apply<1>([&](Record *record) { std::invoke(frecord, record); });
    });
  }
} // namespace

void transition(const Input &n, Bindings &b) { b.set(n.var, Value::top()); }
void transition(const Local &n, Bindings &b) { b.set(n.var, Value::bottom()); }
void transition(const StoreSubscript &n, Bindings &b) {
  update(
      &Element::set_to_bottom,
      [&](Record *record) {
        switch (const auto &subscript{element(b, n.subscript)};
                subscript.kind()) {
        case Kind::Bottom: record->set_to_bottom(); break;
        case Kind::Top: record->set_to_top(); break;
        case Kind::Value: record->add(*subscript.get_constant()); break;
        }
      },
      b, n.target);
}
void transition(const LoadSubscript &n, Bindings &b) {
  update(
      (record(b, n.source).is_bottom() or element(b, n.subscript).is_bottom())
          ? &Element::set_to_bottom
          : &Element::set_to_top,
      &Record::set_to_bottom, b, n.lhs);
}
void transition(const AssignLiteral &n, Bindings &b) {
  auto set_to_literal = [&](Element *e) { *e = Element{n.literal}; };
  update(set_to_literal, &Record::set_to_bottom, b, n.lhs);
}
void transition(const AssignVar &n, Bindings &b) { b.set(n.lhs, b.get(n.rhs)); }
} // namespace ksar

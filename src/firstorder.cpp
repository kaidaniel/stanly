#include "firstorder.h"

namespace ksar {
using namespace syntax;
using namespace domain;
using Kind = sparta::AbstractValueKind;

void transition(const Input &n, Bindings &b) { b.set(n.var, Value::top()); }
void transition(const Local &n, Bindings &b) { b.set(n.var, Value::bottom()); }
void transition(const StoreSubscript &n, Bindings &b) {
  update(
      [&](Element *el) { el->set_to_bottom(); },
      [&](Record *record) {
        switch (auto &subscript{element(b, n.subscript)}; subscript.kind()) {
        case Kind::Bottom: record->set_to_bottom(); break;
        case Kind::Top: record->set_to_top(); break;
        case Kind::Value: record->add(*subscript.get_constant()); break;
        }
      },
      b, n.target);
}

void transition(const LoadSubscript &n, Bindings &b) {
  bool any_bottom =
      record(b, n.source).is_bottom() | element(b, n.subscript).is_bottom();
  auto f = any_bottom ? &Element::set_to_bottom : &Element::set_to_top;
  update(
      [&](Element *el) { (el->*f)(); }, [](Record *re) { re->set_to_bottom(); },
      b, n.lhs);
};

void transition(const AssignLiteral &n, Bindings &b) {
  update(
      [&](Element *el) { *el = Element{n.literal}; },
      [](Record *re) { re->set_to_bottom(); }, b, n.lhs);
};
void transition(const AssignVar &n, Bindings &b) {
  b.set(n.lhs, b.get(n.rhs));
};
} // namespace ksar

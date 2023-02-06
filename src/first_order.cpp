#include "first_order.h"

namespace ksar {
using Var = const syntax::Var;
using Input = const syntax::Input;
using Local = const syntax::Local;
using StoreSubscript = const syntax::StoreSubscript;
using LoadSubscript = const syntax::LoadSubscript;
using AssignLiteral = const syntax::AssignLiteral;
using AssignVar = const syntax::AssignVar;
using Kind = sparta::AbstractValueKind;
using domain::Bindings;
using domain::Element;
using domain::Record;
using domain::Value;

void transition(Input &n, Bindings &b) { b.set(n.var, Value::top()); }
void transition(Local &n, Bindings &b) { b.set(n.var, Value::bottom()); }
void transition(StoreSubscript &n, Bindings &b) {
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

void transition(LoadSubscript &n, Bindings &b) {
  bool any_bottom =
      record(b, n.source).is_bottom() | element(b, n.subscript).is_bottom();
  auto f = any_bottom ? &Element::set_to_bottom : &Element::set_to_top;
  update(
      [&](Element *el) { (el->*f)(); }, [](Record *re) { re->set_to_bottom(); },
      b, n.lhs);
};

void transition(AssignLiteral &n, Bindings &b) {
  update(
      [&](Element *el) { *el = Element{n.literal}; },
      [](Record *re) { re->set_to_bottom(); }, b, n.lhs);
};
void transition(AssignVar &n, Bindings &b) { b.set(n.lhs, b.get(n.rhs)); };
} // namespace ksar

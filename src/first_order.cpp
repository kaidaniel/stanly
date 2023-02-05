#include "first_order.h"

#include <cstdlib>

#include "AbstractDomain.h"

using Var = const concrete::Var;
using Input = const syntax::Input;
using Local = const syntax::Local;
using StoreSubscript = const syntax::StoreSubscript;
using LoadSubscript = const syntax::LoadSubscript;
using AssignScalar = const syntax::AssignScalar;
using AssignVar = const syntax::AssignVar;
using Kind = sparta::AbstractValueKind;

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

namespace concrete { // TODO: use template instead of namespace?
void transition(Input &n, concrete::Bindings &b) {
  int rand{std::rand()};
  b[n.var] =
      rand % 2 == 0
          ? Value{Scalar{rand % 1024}}
          : Value{Object{{rand % 5, rand % 239}, {rand % 7, rand % 587}}};
}
void transition(Local &n, concrete::Bindings &b) {} // no-op
void transition(StoreSubscript &n, concrete::Bindings &b) {
  auto scalar = [&](Scalar &) { return Object{{n.subscript, n.rhs}}; };
  auto object = [&](Object &object) {
    return object.insert_or_assign(n.subscript, n.rhs), object;
  };
  std::visit(overloaded{scalar, object}, b.at(n.target));
}
void transition(LoadSubscript &n, concrete::Bindings &b) {
  auto &subscript = std::get<Scalar>(b.at(n.subscript));
  auto &load_from = std::get<Object>(b.at(n.source));
  b.insert_or_assign(n.lhs, load_from.at(subscript));
}
void transition(AssignScalar &n, concrete::Bindings &b) {
  b.insert_or_assign(n.lhs, n.scalar);
};
void transition(AssignVar &n, concrete::Bindings &b) {
  b.insert_or_assign(n.lhs, b.at(n.rhs));
};
} // namespace concrete

namespace abstract {

void transition(Input &n, Bindings &b) { b.set(n.var, Value::top()); }
void transition(Local &n, Bindings &b) { b.set(n.var, Value::bottom()); }
void transition(StoreSubscript &n, Bindings &b) {
  b.update(n.target, [&](Value *target) {
    target->apply<Value::ObjectIdx>([&](Object *object) {
      switch (auto &scalar = b.get(n.subscript).get<Value::ScalarIdx>();
              scalar.kind()) {
      case Kind::Bottom: object->set_to_bottom(); break;
      case Kind::Top: object->set_to_top(); break;
      case Kind::Value: object->add(*scalar.get_constant()); break;
      }
    });
    target->apply<Value::ScalarIdx>(
        [&](Scalar *scalar) { scalar->set_to_bottom(); });
  });
}
void transition(LoadSubscript &n, Bindings &b) {
  Scalar scalar = (b.get(n.source).get<Value::ObjectIdx>().is_bottom() |
                   b.get(n.subscript).get<Value::ScalarIdx>().is_bottom())
                      ? Scalar::bottom()
                      : Scalar::top();
  b.set(n.lhs, Value{{scalar, Object::bottom()}});
};

void transition(AssignScalar &n, abstract::Bindings &b) {
  b.set(n.lhs, Value{{Scalar{n.scalar}, Object{}}});
};
void transition(AssignVar &n, abstract::Bindings &b) {
  b.set(n.lhs, b.get(n.rhs));
};
} // namespace abstract

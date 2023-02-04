#include "first_order.h"

using Var = const concrete::Var;
using Input = const syntax::Input;
using Local = const syntax::Local;
using StoreSubscript = const syntax::StoreSubscript;
using LoadSubscript = const syntax::LoadSubscript;
using AssignScalar = const syntax::AssignScalar;
using AssignVar = const syntax::AssignVar;

namespace abstract {
Bindings::Bindings(Bindings::Impl &&impl)
    : impl_{std::forward<Bindings::Impl>(impl)} {}
void Bindings::bind(Var &var, const Value &value) { impl_.set(var, value); }
void Bindings::bind_top(Var &var) { bind(var, Value::top()); }
void Bindings::bind_bottom(Var &var) { bind(var, Value::bottom()); }

namespace {
void input(Bindings &b, Var &var) { b.bind_top(var); }
void local(Bindings &b, Var &var) { b.bind_bottom(var); }
void store_subscript(Bindings &b, Var scalar, Var object, Var subscript) {
  throw "not implemented";
}
void load_subscript(Bindings &b, Var scalar, Var object, Var subscript) {
  throw "not implemented";
}
void assign_scalar(Bindings &b, Var var, const concrete::Scalar scalar) {
  throw "not implemented";
}
void assign_var(Bindings &b, Var source, Var target) {
  throw "not implemented";
}
}  // namespace

void transition(Input &n, Bindings &b) { input(b, n.var); }
void transition(Local &n, Bindings &b) { local(b, n.var); }
void transition(StoreSubscript &n, Bindings &b) {
  store_subscript(b, n.scalar, n.object, n.subscript);
}
void transition(LoadSubscript &n, Bindings &b) {
  load_subscript(b, n.scalar, n.object, n.subscript);
}
void transition(AssignScalar &n, Bindings &b) {
  assign_scalar(b, n.var, n.scalar);
};
void transition(AssignVar &n, Bindings &b) {
  assign_var(b, n.source, n.target);
};
}  // namespace abstract

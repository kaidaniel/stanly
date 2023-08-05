#include "analyse.h"

#include <AbstractDomain.h>

#include <unordered_set>
#include <variant>
#include <vector>

#include "domain.h"
#include "handle.h"
#include "stanly-assert.h"
#include "stanly-utils.h"
#include "syntax.h"

namespace stanly {
using enum RowVarEls;
void
analyse(const alloc& alloc, state* d) {
  (*d)([&](scope* s) { s->set(alloc.var, pointer{{addresses{alloc.var}, {}}}); });
  (*d)([&](memory* m) {
    m->set(alloc.var, object{{type{alloc.type}, {}, row_var{Closed}, {}, {}}});
  });
}
void
analyse(const lit& lit, state* d) {
  (*d)([&](scope* s) { s->set(lit.var, pointer{{addresses{lit.value}, {}}}); });
  (*d)([&](memory* m) {
    m->set(lit.value, object{{type{lit.type}, constant{lit.value}, row_var{Closed}, {}, {}}});
  });
}
// ref(.var .src)
// .var = &.src
//
// scp[.var] := scp[.src]
void
analyse(const ref& ref, state* d) {
  (*d)([&](scope* s) { s->set(ref.var, pointer{{addresses{ref.src}, {}}}); });
}
// field(.var .src .field)
// .var = .src[.field]
//
// scp[.var] := union { mem[s].defined[mem[f].const] : s in .src.addrs, f in .field.addrs }
// forall s in .src.addrs, f in .field.addrs:
//   mem[s].used &= mem[f].const;
void
analyse(const read& read, state* d) {
  using enum sparta::AbstractValueKind;
  using enum RowVarEls;
  bool invalid_state = false;
  // clang-format off
  (*d)([&](memory* mem){
  (*d)([&](scope* scp){
  scp->update(read.var,  [&](pointer* var){
  const addresses& field = scp->get(read.field).dereference(*mem);
  const addresses& src = scp->get(read.src).dereference(*mem);
    switch(src.kind()){
      case Top: var->set_to_top(); return;
      case Bottom: invalid_state = true; return;
      case Value:
        for(const handle source : src.elements()){
          mem->update(source, [&](object* obj){
            (*obj)([&](used* u){ u->join_with(field);});
            switch(field.kind()){
              case Top: var->set_to_top(); return;
              case Bottom: return;
              case Value:
                var->set_to_bottom();
                for(const handle fld : field.elements()){
                  const addresses& field_obj_addrs = obj->get<defined>().get(fld);
                    switch(field_obj_addrs.kind()){
                      case Bottom:
                        switch(obj->get<row_var>().element()){
                          case Closed: invalid_state = true; return;  // no binding exists
                          case Open: var->set_to_top(); return;}  // any binding could exist
                      default: var->join_with(pointer{{field_obj_addrs, {}}}); break; }
                  if(invalid_state){ return; } }}});
          if (invalid_state){ return; }}}});
  if (invalid_state){
    mem->set_to_top();
    scp->set_to_bottom(); }});});
  // clang-format on
}
// update(.var .field .src)
// .var[.field] = .src
// *(var + field) = src  (var: T*; field: T::*; src: T)
// equivalent to: *x = src  (x: )
//
// forall v in .var, f in .field:
//   mem[v].defined[mem[f].const] = scp[.src]
//
// update(.var .src) => { mem[v] : v in scp[.var] } := mem[.src]
// ref   (.var .src) => scp[.var] = scp[.src]
//
void
analyse(const update& update, state* d) {
  const auto& scp = d->get<scope>();
  const auto& mem = d->get<memory>();
  const addresses& fields = scp.get(update.field).dereference(mem);
  const auto& elements = fields.is_value() ? fields.elements() : std::unordered_set<handle>{};
  auto var = scp.get(update.var).dereference(mem);
  if (!var.is_value()) { return; }
  (*d)([&](memory* m) {
    for (const handle target : var.elements()) {
      m->update(target, ([&](object* o) {
                  (*o)([&](defined* def) {
                    if (fields.is_top()) { def->set_to_top(); }
                    for (const handle field : elements) {
                      def->set(field, scp.get(update.src).dereference(mem));
                    }
                  });
                }));
    }
  });
}

template <class T>
void
analyse(const T&, state*) {
  unreachable(std::format("analyse({}, {}*) not yet implemented", type_name<T>, type_name<state>));
}

state
analyse(const std::vector<node>& basic_block) {
  state state{};
  for (const auto& node : basic_block) {
    std::visit([&](const auto& n) { analyse(n, &state); }, node);
  }
  return state;
}

state
analyse(const std::vector<basic_block>& graph) {
  stanly_assert(graph.size() == 1);
  return analyse(graph.back().nodes);
}

}  // namespace stanly
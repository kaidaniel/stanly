#include "analyse.h"

#include <AbstractDomain.h>

#include <variant>
#include <vector>

#include "domain.h"
#include "handle.h"
#include "syntax.h"

namespace stanly {

void
analyse(const alloc& alloc, state* d) {
  (*d)([&](scope* s) { s->set(alloc.var, addresses{alloc.var}); });
  (*d)([&](memory* m) {
    m->set(
        alloc.var, object{
                       {type{alloc.type},
                        data{{record{{row_var{RowVarEls::Closed}, defined{}, used{}}}, {}}}}});
  });
}
void
analyse(const lit& lit, state* d) {
  (*d)([&](scope* s) { s->set(lit.var, addresses{lit.value}); });
  (*d)([&](memory* m) {
    m->set(lit.value, object{{type{lit.type}, data{{{}, constant{lit.value}}}}});
  });
}
void
analyse(const ref& ref, state* d) {
  (*d)([&](scope* s) { s->set(ref.var, addresses{ref.src}); });
}
void
analyse(const load& load, state* d) {
  using enum sparta::AbstractValueKind;
  using enum RowVarEls;
  bool invalid_state = false;
  // clang-format off
  (*d)([&](memory* mem){
  (*d)([&](scope* scp){
  scp->update(load.var,  [&](addresses* var){
  const addresses& field = scp->get(load.field);
  const addresses& src = scp->get(load.src);
    switch(src.kind()){
      case Top: var->set_to_top(); return;
      case Bottom: invalid_state = true; return;
      case Value:
        for(const handle source : src.elements()){
          mem->update(source, [&](object* obj){(*obj)([&](data* dt){(*dt)([&](record* src_record){
            (*src_record)([&](used* u){ u->join_with(field);});
            switch(field.kind()){
              case Top: var->set_to_top(); return;
              case Bottom: return;
              case Value:
                var->set_to_bottom();
                for(const handle fld : field.elements()){
                  const addresses& field_obj_addrs = src_record->get<defined>().get(fld);
                    switch(field_obj_addrs.kind()){
                      case Bottom:
                        switch(src_record->get<row_var>().element()){
                          case Closed: invalid_state = true; return;  // no binding exists
                          case Open: var->set_to_top(); return;}  // any binding could exist
                      default: var->join_with(field_obj_addrs); break; }
                  if(invalid_state){ return; } }}});});});
          if (invalid_state){ return; }}}});
  if (invalid_state){
    mem->set_to_top();
    scp->set_to_bottom(); }});});
  // clang-format on
}

void
analyse(const update& update, state* d) {
  const auto& scp = d->get<scope>();
  const addresses& fields = scp.get(update.field);
  const auto& elements = fields.is_value() ? fields.elements() : std::unordered_set<handle>{};
  if (!scp.get(update.var).is_value()) { return; }
  (*d)([&](memory* m) {
    for (const handle target : scp.get(update.var).elements()) {
      m->update(target, [&](object* o) {
        (*o)([&](data* dt) {
          (*dt)([&](record* r) {
            (*r)([&](defined* def) {
              if (fields.is_top()) { def->set_to_top(); }
              for (const handle field : elements) { def->set(field, scp.get(update.src)); }
            });
          });
        });
      });
    }
  });
}

template <class T>
void
analyse(const T&, state*) {
  unreachable(std::format("analyse({}, {}*) not yet implemented", type_name<T>, type_name<state>));
}

state
analyse(const std::vector<node>& graph) {
  state state{};
  for (const auto& node : graph) {
    std::visit([&](const auto& n) { analyse(n, &state); }, node);
  }
  return state;
}
}  // namespace stanly
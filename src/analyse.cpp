#include "analyse.h"

#include <AbstractDomain.h>

#include <variant>
#include <vector>

#include "domain.h"
#include "handle.h"
#include "syntax.h"

namespace stanly {
namespace domains {
std::ostream& operator<<(std::ostream& os, RowVarEls rve) {
  switch (rve) {
    case RowVarEls::Closed: os << "Closed"; break;
    case RowVarEls::Open: os << "Open"; break;
  };
  return os;
}

}  // namespace domains
using namespace domains;
using namespace syntax;

object join_addresses(const var_repr var, state* s) {
  using enum sparta::AbstractValueKind;
  const scope& scp = s->get<state::idx<scope>>();
  const memory& mem = s->get<state::idx<memory>>();
  const addresses& addresses = scp.get(var);
  object src = object::bottom();
  if (addresses.is_top()) { src.set_to_top(); }
  if (addresses.is_value()) {
    for (const address_repr& a : addresses.elements()) {
      src.join_with(mem.get(a));
      if (src.is_top()) { break; }
    }
  }
  return src;
}

void analyse(const alloc& alloc, domain* d) {
  d->template set_key<scope>(alloc.var, addresses{alloc.var});
  d->template set_key<memory>(alloc.var, object{{type{alloc.type}, data{record{{}}}}});
}
void analyse(const lit& lit, domain* d) {
  d->template set_key<scope>(lit.var, addresses{lit.value});
  d->template set_key<memory>(lit.value, object{{type{lit.type}, data{constant{lit.value}}}});
}
void analyse(const ref& ref, domain* d) { d->template set_key<scope>(ref.var, addresses{ref.src}); }
void analyse(const load& load, domain* d) {
  // TODO: make the commented-out version work instead
  // src = ...
  // field = ...
  // var = src[field]
  // using enum sparta::AbstractValueKind;
  // const scope& scp = d->get<domain::idx<scope>>();
  // const addresses& possible_sources = scp.get(load.src);
  // const addresses& possible_fields = scp.get(load.field);

  // if (possible_sources.is_top()) { 
  //   d->apply<domain::idx<scope>>([&](scope* s){ s->set(load.var, top);});
  //   return;
  // }
  // if (possible_sources.is_bottom()){
  //   d->apply<domain::idx<scope>>([&](scope* s){ s->set(load.var, bot); });
  //   return;
  // }
  // for (const address_repr source : possible_sources.elements()) {
  //   auto set_addr = [&](auto f) { d->apply<domain::idx<memory>>([&](memory* m){ m->update(source, f);});};
  //   switch(possible_fields.kind()){
  //     case Top: set_addr([&](auto* addrs) { addrs->set_to_top();}); break;
  //     case Bottom: set_addr([&](auto* addrs) { addrs->set_to_bottom();}); break;
  //     case Value: set_addr([&](addresses* addrs){ addrs->add(possible_fields.elements().begin(), possible_fields.elements().end());}); break;
  //   }
  //   if (possible_fields.is_top()){
  //     d->apply<domain::idx<memory>>([&](memory* m){
  //       m->update(source, [&](addresses* addrs){ addrs->set_to_top();});
  //     });
  //   }
  // }



  // 

  const constant field = join_addresses(load.field, d)
                             .get<object::idx<data>>()
                             .maybe_get<constant>()
                             .get_value_or(bot);

  d->apply<state::idx<scope>>([&](scope* s) {
    using enum sparta::AbstractValueKind;
    using enum RowVarEls;
    // Record that `src` used `field`.
    // forall addr in scope[load.src]:
    //     memory.set(
    //       (address) addr,
    //       (object) case memory.get(addr), field:
    //           1) object(t, record(_, def, used)), top -> object(t, record(*, def, used))
    //           2) _,                               bot -> bot
    //           3) object(t, record(r, def, used)), value(x) -> object(t, record(r, def, used ::
    //           x)) 4) object(_, constant(_)),          _ -> bot
    //     )
    s->update(load.src, [&](addresses* addrs) {
      if (addrs->is_bottom()) { return; }
      stanly_assert(addrs->is_value(),
                    "didn't implement loading field of variable that could be any address");
      for (const auto a : addrs->elements()) {
        d->apply<state::idx<memory>>([&](memory* m) {
          m->update(a, [&](object* o) {
            o->apply<object::idx<data>>([&](data* dt) {
              dt->apply<constant>([&](constant*) { o->set_to_bottom(); });  // 4)
              switch (field.kind()) {
                case Bottom: o->set_to_bottom(); break;  // 2)
                case Top:                                // 1)
                  dt->apply<record>([&](record* r) {
                    r->apply<record::idx<row_var>>([&](row_var* rv) { rv->set_to_top(); });
                  });
                  break;
                case Value:  // 3)
                  dt->apply<record>([&](record* r) {
                    r->apply<record::idx<used>>([&](used* u) { u->add(*field.get_constant()); });
                  });
                  break;
              }
            });
          });
        });
      };
    });
    // Assign object `var` to `load.var`
    // scope.set(
    //   (var_handle) load.var,
    //   (addresses)  case join_addresses(load.src, d), field:
    //       _, bot -> bot
    //       object(_, constant(_)),                  _ -> bot
    //       object(_, record(closed(), def, _)),     top -> union(def[f] for f in def.fields)
    //       object(_, record(open(), _, _)),         top -> top
    //       object(_, record(_, def{f: k, ...}, _)), constant(f) -> k   // where top[f] = top and
    //       bot[f] = bot object(_, record(closed(), def, _)),     constant(f) -> bot object(_,
    //       record(open(), def, _)),       constant(f) -> top
    object src = join_addresses(load.src, d);
    s->update(load.var, [&](addresses* addrs) {
      src.apply<object::idx<data>>([&](data* dt) {
        if (field.is_bottom()) {
          addrs->set_to_bottom();
          return;
        }
        dt->apply<constant>([&](constant*) { addrs->set_to_bottom(); });
        dt->apply<record>([&](record* r) {
          const RowVarEls rv = r->get<record::idx<row_var>>().element();
          const defined& def = r->get<record::idx<defined>>();
          if (def.is_top()) {
            addrs->set_to_top();
            return;
          }
          if (field.is_top()) {
            if (rv == Open) {
              addrs->set_to_top();
              return;
            }
            *addrs = addresses{};
            for (const auto& [var, value] : def.bindings()) {
              if (value.is_top()) {
                addrs->set_to_top();
                break;
              }
              if (value.is_value()) {
                addrs->add(value.elements().begin(), value.elements().end());
              }
            }
            return;
          }
          // field.is_value() b/c field.is_bottom() returns earlier.
          const constant_repr& field_v = *field.get_constant();
          if (def.bindings().contains(field_v)) {
            *addrs = def.bindings().at(field_v);
            return;
          }
          switch (rv) {
            case Open: addrs->set_to_top(); break;
            case Closed: addrs->set_to_bottom(); break;
          }
        });
      });
    });
  });
}
void analyse(const update& update, domain* d) {
  // tgt = ...
  // src = ...
  // field = ...
  // tgt[field] = src
  using enum sparta::AbstractValueKind;
  const scope& scp = d->get<domain::idx<scope>>();
  const addresses& possible_sources = scp.get(update.src);
  const addresses& possible_fields = scp.get(update.field);
  const addresses& possible_targets = scp.get(update.tgt);
  if (!possible_targets.is_value()) { return; }
  for (const address_repr target : possible_targets.elements()) {
    d->apply<domain::idx<memory>>([&](memory* m) {
      m->update(target, [&](object* o) {
        switch (possible_fields.kind()) {
          case Top: o->set_to_top(); break;
          case Bottom: o->set_to_bottom(); break;
          case Value:
            o->apply<object::idx<data>>([&](data* dt) {
              dt->apply<record>([&](record* r) {
                r->apply<record::idx<defined>>([&](defined* def) {
                  for (const address_repr field : possible_fields.elements()) {
                    def->set(field, possible_sources);
                  }
                });
              });
            });
        }
      });
    });
  }
}
domain analyse(const std::vector<firstorder>& graph) {
  domain domain{};
  for (const auto& node : graph) {
    std::visit([&](const auto& n) { analyse(n, &domain); }, node);
  }
  return domain;
}

}  // namespace stanly

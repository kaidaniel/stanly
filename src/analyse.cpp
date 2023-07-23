#include "analyse.h"

#include <AbstractDomain.h>

#include <variant>
#include <vector>

#include "domain.h"
#include "handle.h"
#include "syntax.h"

namespace stanly {
std::ostream&
operator<<(std::ostream& os, RowVarEls rve) {
  switch (rve) {
    case RowVarEls::Closed: os << "Closed"; break;
    case RowVarEls::Open: os << "Open"; break;
  };
  return os;
}

void
analyse(const alloc& alloc, state* d) {
  d->template set_key<scope>(alloc.var, addresses{alloc.var});
  d->template set_key<memory>(
      alloc.var,
      object{
          {type{alloc.type}, data{{record{{row_var{RowVarEls::Closed}, defined{}, used{}}}, {}}}}});
}
void
analyse(const lit& lit, state* d) {
  d->template set_key<scope>(lit.var, addresses{lit.value});
  d->template set_key<memory>(lit.value, object{{type{lit.type}, data{{{}, constant{lit.value}}}}});
}
void
analyse(const ref& ref, state* d) {
  d->template set_key<scope>(ref.var, addresses{ref.src});
}
void
analyse(const load& load, state* d) {
  using enum sparta::AbstractValueKind;
  using enum RowVarEls;
  const scope& scp = d->get<state::idx<scope>>();
  const addresses& sources = scp.get(load.src);
  const addresses& fields = scp.get(load.field);
  const auto& elements = fields.is_value() ? fields.elements() : std::unordered_set<handle>{};
  bool invalid_state = false;

  auto set_invalid_state = [&]() {
    d->apply<state::idx<memory>>([&](memory* m) { m->set_to_top(); });
    d->apply<state::idx<scope>>([&](scope* s) { s->set_to_bottom(); });
  };

  auto set_load_var = [&](addresses&& x) {
    d->apply<state::idx<scope>>([&](scope* s) { s->set(load.var, x); });
  };
  switch (sources.kind()) {
    case Top: set_load_var(addresses::top()); return;
    case Bottom: set_invalid_state(); return;
    case Value: set_load_var(addresses{}); break;
  }

  for (const address_repr source : sources.elements()) {
    d->apply<state::idx<memory>>([&](memory* m) {
      m->update(source, [&](object* o) {
        o->apply<object::idx<data>>([&](data* dt) {
          dt->apply<data::idx<record>>([&](record* r) {
            r->apply<record::idx<used>>([&](used* u) { u->join_with(fields); });
            r->apply<record::idx<defined>>([&](defined* def) {
              d->apply<state::idx<scope>>([&](scope* scope) {
                scope->update(load.var, [&](addresses* addrs) {
                  if (fields.is_top()) { addrs->set_to_top(); }
                  for (const address_repr field : elements) {
                    def->update(field, [&](addresses* field_addrs) {
                      if (field_addrs->is_bottom()) {  // no binding recorded
                        switch (r->get<record::idx<row_var>>().element()) {
                          case Closed: invalid_state = true; return;  // no binding exists
                          case Open: addrs->set_to_top(); return;     // any binding could exist
                        }
                      }
                      addrs->join_with(*field_addrs);
                    });
                  };
                });
              });
            });
          });
        });
      });
    });
    if (invalid_state) {
      set_invalid_state();
      break;
    }
  }
}

void
analyse(const update& update, state* d) {
  const scope& scp = d->get<state::idx<scope>>();
  const addresses& fields = scp.get(update.field);
  const auto& elements = fields.is_value() ? fields.elements() : std::unordered_set<handle>{};
  if (!scp.get(update.var).is_value()) { return; }
  d->apply<state::idx<memory>>([&](memory* m) {
    for (const address_repr target : scp.get(update.var).elements()) {
      m->update(target, [&](object* o) {
        o->apply<object::idx<data>>([&](data* dt) {
          dt->apply<data::idx<record>>([&](record* r) {
            r->apply<record::idx<defined>>([&](defined* def) {
              if (fields.is_top()) { def->set_to_top(); }
              for (const address_repr field : elements) { def->set(field, scp.get(update.src)); }
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
analyse(const std::vector<ast_node>& graph) {
  state state{};
  for (const auto& node : graph) {
    std::visit([&](const auto& n) { analyse(n, &state); }, node);
  }
  return state;
}
}  // namespace stanly
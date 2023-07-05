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
  d->template set_key<memory>(
      alloc.var,
      object{{type{alloc.type}, data{record{{row_var{RowVarEls::Closed}, defined{}, used{}}}}}});
}
void analyse(const lit& lit, domain* d) {
  d->template set_key<scope>(lit.var, addresses{lit.value});
  d->template set_key<memory>(lit.value, object{{type{lit.type}, data{constant{lit.value}}}});
}
void analyse(const ref& ref, domain* d) { d->template set_key<scope>(ref.var, addresses{ref.src}); }
void analyse(const load& load, domain* d) {
  using enum sparta::AbstractValueKind;
  using enum RowVarEls;
  const scope& scp = d->get<domain::idx<scope>>();
  const addresses& sources = scp.get(load.src);
  const addresses& fields = scp.get(load.field);
  const auto& elements = fields.is_value() ? fields.elements() : std::unordered_set<handle>{};
  bool invalid_state = false;

  auto set_invalid_state = [&]() {
    d->apply<domain::idx<memory>>([&](memory* m) { m->set_to_top(); });
    d->apply<domain::idx<scope>>([&](scope* s) { s->set_to_bottom(); });
  };

  auto set_load_var = [&](addresses&& x) {
    d->apply<domain::idx<scope>>([&](scope* s) { s->set(load.var, x); });
  };
  switch (sources.kind()) {
    case Top: set_load_var(top); return;
    case Bottom: set_invalid_state(); return;
    case Value: set_load_var(addresses{}); break;
  }

  for (const address_repr source : sources.elements()) {
    d->apply<domain::idx<memory>>([&](memory* m) {
      m->update(source, [&](object* o) {
        o->apply<object::idx<data>>([&](data* dt) {
          dt->apply<record>([&](record* r) {
            r->apply<record::idx<used>>([&](used* u) { u->join_with(fields); });
            r->apply<record::idx<defined>>([&](defined* def) {
              d->apply<domain::idx<scope>>([&](scope* scope) {
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

void analyse(const update& update, domain* d) {
  const scope& scp = d->get<domain::idx<scope>>();
  const addresses& fields = scp.get(update.field);
  const auto& elements = fields.is_value() ? fields.elements() : std::unordered_set<handle>{};
  if (!scp.get(update.tgt).is_value()) { return; }
  d->apply<domain::idx<memory>>([&](memory* m) {
    for (const address_repr target : scp.get(update.tgt).elements()) {
      m->update(target, [&](object* o) {
        o->apply<object::idx<data>>([&](data* dt) {
          dt->apply<record>([&](record* r) {
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
domain analyse(const std::vector<firstorder>& graph) {
  domain domain{};
  for (const auto& node : graph) {
    std::visit([&](const auto& n) { analyse(n, &domain); }, node);
  }
  return domain;
}

}  // namespace stanly

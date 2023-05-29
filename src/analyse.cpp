#include "analyse.h"

#include <variant>
#include <vector>

#include "domain.h"
#include "handle.h"
#include "syntax.h"

namespace stanly {
namespace domains {
std::ostream& operator<<(std::ostream& os, RowVarEls rve) {
  switch (rve) {
    case RowVarEls::Bot: os << "Bot"; break;
    case RowVarEls::Closed: os << "Closed"; break;
    case RowVarEls::Open: os << "Open"; break;
  };
  return os;
}

}  // namespace domains
using namespace domains;
using namespace syntax;
void analyse(const alloc& alloc, domain* d) {
  d->template set_key<scope>(alloc.var, addresses{alloc.var});
  d->template set_key<memory>(alloc.var, object{{type{alloc.type}, data::bottom()}});
}
void analyse(const lit& lit, domain* d) {
  d->template set_key<scope>(lit.var, addresses{lit.var});
  d->template set_key<memory>(lit.var, object{{type{lit.type}, data{constant{lit.value}}}});
}
void analyse(const ref& ref, domain* d) { d->template set_key<scope>(ref.var, addresses{ref.src}); }
void analyse(const load& load, domain* d) {
  d->template set_key<scope>(load.var, addresses{load.var});
  d->add_used_field(load.var, load.field);
}
void analyse(const update& update, domain* d) {
  d->define_field(update.src, update.field, update.tgt);
}
domain analyse(const std::vector<firstorder>& graph) {
  domain domain;
  for (const auto& node : graph) {
    std::visit([&](const auto& n) { analyse(n, &domain); }, node);
  }
  return domain;
}

}  // namespace stanly
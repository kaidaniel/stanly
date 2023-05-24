#include "analyse.h"

#include <vector>

#include "domain.h"
#include "syntax.h"

namespace stanly {
namespace detail {
std::ostream& operator<<(std::ostream& os, RowVarEls rve) {
  switch (rve) {
    case RowVarEls::Bot: os << "Bot"; break;
    case RowVarEls::Closed: os << "Closed"; break;
    case RowVarEls::Open: os << "Open"; break;
  };
  return os;
}

}  // namespace detail

domain<std::string_view> analyse(const std::vector<nodes::firstorder>& graph) { return {}; }

struct analysis : public lang<handle>, public domains<handle> {
  static void analyse(const alloc& alloc, domain<handle>* d) {
    d->set_key<scope>(alloc.var, addresses{alloc.var});
    d->set_key<memory>(alloc.var, object{{type{alloc.type}, data::bottom()}});
  }
  static void analyse(const ref& ref, domain<handle>* d) {
    d->set_key<scope>(ref.var, addresses{ref.src});
  }
  static void analyse(const lit& lit, domain<handle>* d) {
    d->set_key<scope>(lit.var, addresses{lit.var});
    // d->set_key<memory>(lit.var, object{{type{lit.type}, data{constant{lit.value}}}});
  }
};
}  // namespace stanly
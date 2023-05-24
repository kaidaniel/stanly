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

domain analyse(const std::vector<nodes::firstorder>& graph) { return {}; }

void analyse(const nodes::alloc& alloc, domain* domain) {}
}  // namespace stanly
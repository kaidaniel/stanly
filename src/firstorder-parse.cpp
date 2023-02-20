#include "firstorder-lang.h"
#include "stanly-api.h"

namespace stanly {
GraphType parse_firstorder(const std::string &) {
  return GraphType{FirstOrderGraph{}};
};
} // namespace stanly
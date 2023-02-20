#include "firstorder-lang.h"
#include "stanly-api.h"
#include <vector>
#include <string>
#include <algorithm>


namespace stanly {

template<class ...Args>
void FirstOrderGraph::insert(Args&&... args) {nodes.emplace_back(args...);};
std::string show(const FirstOrderGraph &) { return "FirstOrderGraph"; }


Graph parse_firstorder(const std::string&) {
  return Graph{FirstOrderGraph{}};
};
} // namespace stanly

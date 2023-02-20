#include "firstorder-lang.h"
#include "stanly-api.h"

namespace stanly {
std::string show(const FirstOrderGraph &) { return "FirstOrderGraph"; }
AnalysisType analyse(const FirstOrderGraph &) {
  return AnalysisType{FirstOrderAnalysis{}};
}
} // namespace stanly

#include <string>
#include <vector>

#include "syntax.h"

namespace stanly {
std::vector<node> parse(std::string&& program);
}  // namespace stanly
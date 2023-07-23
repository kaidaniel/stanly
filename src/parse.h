#include <string>
#include <vector>

#include "syntax.h"

namespace stanly {
std::vector<ast_node> parse(std::string&& program);
}  // namespace stanly
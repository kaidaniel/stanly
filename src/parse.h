#include <string_view>
#include <vector>

#include "syntax.h"

namespace stanly {
std::vector<syntax::ast_node> parse(std::string_view program);

}
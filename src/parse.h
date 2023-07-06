#include <string_view>
#include <vector>

#include "string_index.h"
#include "syntax.h"

namespace stanly {
std::vector<syntax::ast_node> parse(std::string_view program, string_index = {});
}
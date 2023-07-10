#include <string>
#include <vector>

#include "syntax.h"

namespace stanly {
std::vector<syntax::ast_node>
parse(std::string&& program);

std::string
generate_tree_sitter_symbols();
}  // namespace stanly
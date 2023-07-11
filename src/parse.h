#include <string>
#include <vector>

#include "syntax.h"

namespace stanly {
std::vector<syntax::ast_node>
parse(std::string&& program);

std::string generate_tree_sitter_symbols(std::string_view);

std::string
show_surface_syntax(std::string_view program);
}  // namespace stanly
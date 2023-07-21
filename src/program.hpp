#pragma once

#include <vector>
#include "syntax.h"

namespace stanly::parser{
struct program {
    std::vector<syntax::ast_node> nodes;
    std::vector<syntax::basic_block> basic_blocks;
};
}

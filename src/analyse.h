#pragma once

#include <vector>

#include "domain.h"
#include "syntax.h"

namespace stanly {

state analyse(const std::vector<ast_node>&);
}  // namespace stanly
#pragma once

#include <vector>

#include "domain.h"
#include "syntax.h"

namespace stanly {

domain analyse(const std::vector<nodes::firstorder>&);
}  // namespace stanly
#pragma once
#include "stanly-api.h"
#include <string_view>

namespace stanly {
Graph parse_firstorder(std::string_view);
}

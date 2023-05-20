#include <string_view>
#include <vector>

#include "syntax.h"

namespace stanly {
template <syntax S>
std::vector<S> parse(std::string_view program);
}
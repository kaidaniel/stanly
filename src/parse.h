#include <string_view>
#include <vector>

#include "syntax.h"

namespace stanly {
template <ast S>
std::vector<S> parse(std::string_view program);
}
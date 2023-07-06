#include <string_view>
#include <vector>

#include "string_index.h"
#include "syntax.h"

namespace stanly {
std::vector<syntax::firstorder> parse(std::string_view program, StringIndex = {});
}
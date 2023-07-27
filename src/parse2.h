#include <string_view>

#include "stanly-concepts.h"

namespace stanly {
template <parser_c Parser>
std::decay_t<decltype(Parser::program)> parse(std::string&&);
}
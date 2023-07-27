#include <string>

#include "stanly-concepts.h"  // IWYU pragma: keep (clangd bug with concepts: https://github.com/llvm/llvm-project/issues/60702)

namespace stanly {
template <parser_c Parser>
decltype(Parser::program) parse(std::string&&);
}
#include <string>
#include <utility>

#include "cursor.cpp"
#include "parse2.cpp"
#include "program.h"
#include "stanly-concepts.h"  // IWYU pragma: keep (clangd bug with concepts: https://github.com/llvm/llvm-project/issues/60702)

namespace stanly {
static_assert(cursor_c<cursor>);
static_assert(program_c<program>);

struct parser {
  parser(std::string&& source) : cursor(add_string_to_index(program, std::move(source))) {}
  program program{};
  cursor cursor;
};

static_assert(parser_c<parser>);
inline constexpr auto fns = jump_table<250, parse_symbol_functions<parser>>;
template <>
program parse<parser>(std::string&&);
}  // namespace stanly
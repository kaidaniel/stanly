#pragma once

#include "syntax.h"

namespace stanly{
struct program;
template<ast_cons T>
void make_node(program&, std::same_as<std::string_view> auto... args);
template<basic_block_cons T>
void make_basic_block(program&, std::same_as<std::string_view> auto... args);
}

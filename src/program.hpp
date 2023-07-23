#pragma once

#include "syntax.h"

namespace stanly{
struct program;
void append_node(program&, node&&);
void append_basic_block(program&, basic_block&&);
}

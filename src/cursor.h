#pragma once

#include <string_view>
#include <utility>

namespace stanly {

struct cursor;

std::string_view current_text(cursor&);
uint16_t current_symbol(cursor&);
std::optional<uint16_t> current_field(cursor&);

bool goto_child(cursor&);
bool goto_sibling(cursor&);
void goto_parent(cursor&);
}  // namespace stanly

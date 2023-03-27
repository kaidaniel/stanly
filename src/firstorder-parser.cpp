#include <fmt/core.h>

#include <cassert>
#include <string_view>

#include "firstorder-syntax.h"
#include "parser.h"

namespace stanly {
using fmt::format;
using std::string_view;
using fo = firstorder::syntax<string_view>;
template <>
fo::node parser::next_node<fo>() {
  assert(at(&Symbols::expression_statement));
  to_child();
  assert(at(&Symbols::assignment));
  to_child();
  assert(at(&Fields::left, &Symbols::identifier));
  fo::repr left{text()};
  to_sibling();
  assert(at(&Fields::right));
  fo::repr right{text()};
  // clang-format off
  if (at(&Symbols::identifier)){ return fo::load_var   {left, right}; }
  if (at(&Symbols::string)){     return fo::load_text  {left, right}; }
  if (at(&Symbols::integer)){    return fo::load_text  {left, right}; }
  if (at(&Symbols::dictionary)){ return fo::load_record{left, record()}; }
  if (at(&Symbols::set)){        return fo::load_record{left, record()}; }
  if (at(&Symbols::list)){       return fo::load_top   {left, right}; }
  // clang-format on
  throw std::domain_error(format("assigning ({} {}) not implemented", type(), right));
}
template <>
iterator::inpt_range<fo::node> parse<fo>(string_view program) {
  return {&parser::next_node<fo>, &parser::is_done, program};
}
}  // namespace stanly

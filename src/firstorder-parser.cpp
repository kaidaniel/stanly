#include <cassert>
#include <format>
#include <string_view>

#include "firstorder-syntax.h"
#include "parser.h"

namespace stanly {
using std::format;
using std::string_view;
using fo = firstorder::syntax<string_view>;
template <>
fo::node parser::parse_statement<fo>() {
  auto unpack_subscript = [this] {
    to_child();
    assert(at(&Fields::value, &Symbols::identifier));
    fo::repr const variable{text()};
    to_sibling();
    assert(at(&Fields::subscript, &Symbols::identifier));
    fo::repr const field{text()};
    return std::tuple{variable, field};
  };
  assert(at(&Symbols::expression_statement));
  to_child();
  assert(at(&Symbols::assignment));
  to_child();
  assert(at(&Fields::left));

  if (at(&Symbols::identifier)) {
    fo::repr const left{text()};
    to_sibling();
    assert(at(&Fields::right));
    fo::repr const right{text()};
    if (at(&Symbols::identifier)) { return fo::load_var{left, right}; }
    if (at(&Symbols::string)) { return fo::load_text{left, right}; }
    if (at(&Symbols::integer)) { return fo::load_text{left, right}; }
    if (at(&Symbols::dictionary)) { return fo::load_record{left, record()}; }
    if (at(&Symbols::set) || at(&Symbols::list)) {
      to_child();
      while (to_sibling()) {};  // ignore children.
      return fo::load_top{left, right};
    }
    if (at(&Symbols::subscript)) {
      auto [variable, field] = unpack_subscript();
      return fo::load_field{left, variable, field};
    }
  }

  if (at(&Symbols::subscript)) {
    auto [variable, field] = unpack_subscript();
    to_parent();
    to_sibling();
    assert(at(&Fields::right, &Symbols::identifier));
    return fo::set_field{text(), variable, field};
  };

  throw std::domain_error(format("assigning ({} {}) not implemented", type(), text()));
}
}  // namespace stanly

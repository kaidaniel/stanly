#include <cassert>
#include <format>
#include <string_view>

#include "firstorder-syntax.h"
#include "parser.h"

namespace stanly {
using std::format;
using std::string_view;
using stx = firstorder::syntax<string_view>;
template <>
stx::node parser::parse_statement<stx>() {
  auto unpack_subscript = [this] {
    to_child();
    assert(at(&Fields::value, &Symbols::identifier));
    stx::repr const variable{text()};
    to_sibling();
    assert(at(&Fields::subscript, &Symbols::identifier));
    stx::repr const field{text()};
    return std::tuple{variable, field};
  };
  assert(at(&Symbols::expression_statement));
  to_child();
  assert(at(&Symbols::assignment));
  to_child();
  assert(at(&Fields::left));

  if (at(&Symbols::identifier)) {
    stx::repr const left{text()};
    to_sibling();
    assert(at(&Fields::right));
    stx::repr const right{text()};
    if (at(&Symbols::identifier)) { return stx::load_var{left, right}; }
    if (at(&Symbols::string)) { return stx::load_text{left, right}; }
    if (at(&Symbols::integer)) { return stx::load_text{left, right}; }
    if (at(&Symbols::dictionary)) { return stx::load_record{left, record()}; }
    if (at(&Symbols::set) || at(&Symbols::list)) {
      to_child();
      while (to_sibling()) {};  // ignore children.
      return stx::load_top{left, right};
    }
    if (at(&Symbols::subscript)) {
      auto [variable, field] = unpack_subscript();
      return stx::load_field{left, variable, field};
    }
  }

  if (at(&Symbols::subscript)) {
    auto [variable, field] = unpack_subscript();
    to_parent();
    to_sibling();
    assert(at(&Fields::right, &Symbols::identifier));
    return stx::set_field{text(), variable, field};
  };

  throw std::domain_error(format("assigning ({} {}) not implemented", type(), text()));
}

template std::vector<stx::node> parse<stx>(string_view);
}  // namespace stanly

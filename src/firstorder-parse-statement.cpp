#include <ranges>
#include <string_view>
#include <vector>

#include "firstorder-syntax.h"
#include "parse.h"
#include "stanly-utils.h"

namespace stanly {
using std::string_view;
using std::vector;
using syn = firstorder::syntax<string_view>;
struct firstorder_cursor : public cursor {
  using cursor::cursor;
  auto parse_dictionary_keys() -> vector<string_view> {
    // dictionary("{" commaSep1(pair | dictionary_splat)? ","? "}")
    stanly_assert(symbol() == symbols.dictionary);  // <dictionary(...)>
    vector<string_view> dictionary_keys{};
    goto_child();                              // dictionary(<'{'> pair(...) ...)
    while (goto_sibling() && text() != "}") {  // dictionary(... <pair(...)> ...)
      // pair(key:expression ":" value:expression)
      stanly_assert(symbol() == symbols.pair);
      dictionary_keys.emplace_back(text(fields.key));
      goto_sibling();  // dictionary(... <','> ...)
    }
    goto_parent();  // <dictionary(...)>
    stanly_assert(symbol() == symbols.dictionary);
    return dictionary_keys;
  }
  auto parse_variable_and_field_from_subscript() -> std::tuple<std::string_view, std::string_view> {
    goto_child();
    stanly_assert(field() == fields.value);
    stanly_assert(symbol() == symbols.identifier);
    auto const variable = text();
    goto_sibling();  // skip '['
    goto_sibling();
    stanly_assert(field() == fields.subscript);
    stanly_assert(symbol() == symbols.identifier);
    return {variable, text()};
  };
  [[nodiscard]] syn::node yield() {
    syn::node node = queue.back();
    queue.pop_back();
    return node;
  }
  [[nodiscard]] syn::node yield(rg::input_range auto... nodes) {
    (rg::copy(nodes, std::back_inserter(queue)), ...);
    return yield();
  }

  auto parse_statement() -> syn::node {
    if (!queue.empty()) { return yield(); };
    stanly_assert(symbol() == symbols.expression_statement);
    goto_child();
    stanly_assert(symbol() == symbols.assignment);
    goto_child();
    stanly_assert(field() == fields.left);

    auto symbol_ = symbol();
    if (symbol_ == symbols.identifier) {
      auto const left = text();
      goto_sibling();  // assignment(left:identifier <"="> ...)
      goto_sibling();  // assignment(left:identifier "=" <right:...>)
      stanly_assert(field() == fields.right);

      symbol_ = symbol();
      auto const right = text();
      if (symbol_ == symbols.identifier) { return syn::ref{left, right}; }
      if (symbol_ == symbols.string || symbol_ == symbols.integer) {
        return syn::text{left, right};
      }
      if (symbol_ == symbols.dictionary) {
        auto mk_update = [=](auto key) { return syn::update(left, key, ""); };
        return yield(std::vector{syn::alloc{left, "dict"}},
                     parse_dictionary_keys() | vw::reverse | vw::transform(mk_update));
      }
      if (symbol_ == symbols.set || symbol_ == symbols.list) { return syn::alloc{left, "top"}; }
      if (symbol_ == symbols.subscript) {
        auto [variable, field_] = parse_variable_and_field_from_subscript();
        return syn::load{left, variable, field_};
      }
      unreachable();
    }

    if (symbol_ == symbols.subscript) {
      auto [variable, field_] = parse_variable_and_field_from_subscript();
      goto_parent();
      goto_sibling();  // skip "="
      goto_sibling();
      stanly_assert(symbol() == symbols.identifier);
      return syn::update{variable, field_, text()};
    };

    unreachable();
  }

 private:
  std::vector<syn::node> queue{};
};
template <>
firstorder::syntax<string_view>::node parse_statement<firstorder::syntax<string_view>::node>(
    TSTreeCursor* cursor, string_view program) {
  return firstorder_cursor{cursor, program}.parse_statement();
};
}  // namespace stanly

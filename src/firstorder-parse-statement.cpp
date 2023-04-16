#include <string_view>
#include <vector>

#include "firstorder-syntax.h"
#include "parse.h"
#include "stanly-utils.h"

namespace stanly {
using std::string_view;
using std::vector;
using stx = firstorder::syntax<string_view>;

namespace parser {
template <>
stx::node parse_statement<stx>(TSTreeCursor* cursor, string_view program) {
  auto node = [&] { return ts_tree_cursor_current_node(cursor); };
  auto symbol = [&] { return ts_node_symbol(node()); };
  auto field = [&] { return ts_tree_cursor_current_field_id(cursor); };
  auto goto_child = [&] { return ts_tree_cursor_goto_first_child(cursor); };

  auto goto_sibling = [&] { return ts_tree_cursor_goto_next_sibling(cursor); };
  auto goto_parent = [&] { return ts_tree_cursor_goto_parent(cursor); };

  auto text = [&](const TSNode node) -> string_view {
    return {std::begin(program) + ts_node_start_byte(node),
            std::begin(program) + ts_node_end_byte(node)};
  };
  auto child_text = [&](TSFieldId field) {
    return text(ts_node_child_by_field_id(ts_tree_cursor_current_node(cursor), field));
  };

  auto parse_dictionary_keys = [&] {
    // dictionary("{" commaSep1(pair | dictionary_splat)? ","? "}")
    stanly_assert(symbol() == symbols.dictionary);  // <dictionary(...)>
    vector<string_view> dictionary_keys{};
    goto_child();                                    // dictionary(<'{'> pair(...) ...)
    while (goto_sibling() && text(node()) != "}") {  // dictionary(... <pair(...)> ...)
      // pair(key:expression ":" value:expression)
      stanly_assert(symbol() == symbols.pair);
      dictionary_keys.emplace_back(child_text(fields.key));
      goto_sibling();  // dictionary(... <','> ...)
    }
    goto_parent();  // <dictionary(...)>
    stanly_assert(symbol() == symbols.dictionary);
    return dictionary_keys;
  };

  auto parse_variable_and_field_from_subscript = [&] {
    goto_child();
    stanly_assert(field() == fields.value);
    stanly_assert(symbol() == symbols.identifier);
    string_view const variable{text(node())};
    goto_sibling();  // skip '['
    goto_sibling();
    stanly_assert(field() == fields.subscript);
    stanly_assert(symbol() == symbols.identifier);
    return std::tuple{variable, text(node())};
  };
  stanly_assert(symbol() == symbols.expression_statement);
  // expression_statement: $ => choice(
  //      $.expression,
  //      seq(commaSep1($.expression), optional(',')),
  //      $.assignment,
  //      $.augmented_assignment,
  //      $.yield
  // ),
  goto_child();
  stanly_assert(symbol() == symbols.assignment);
  //  assignment: $ => seq(
  //    field('left', $._left_hand_side),
  //    choice(
  //        seq('=', field('right', $._right_hand_side)),
  //        seq(':', field('type', $.type)),
  //        seq(':', field('type', $.type), '=', field('right', $._right_hand_side))
  //            )
  //  ),
  goto_child();
  stanly_assert(field() == fields.left);

  auto symbol_ = symbol();
  if (symbol_ == symbols.identifier) {
    std::string_view const left{text(node())};
    goto_sibling();  // assignment(left:identifier <"="> ...)
    goto_sibling();  // assignment(left:identifier "=" <right:...>)
    stanly_assert(field() == fields.right);

    symbol_ = symbol();
    std::string_view const right{text(node())};
    if (symbol_ == symbols.identifier) { return stx::load_var{left, right}; }
    if (symbol_ == symbols.string) { return stx::load_text{left, right}; }
    if (symbol_ == symbols.integer) { return stx::load_text{left, right}; }
    if (symbol_ == symbols.dictionary) { return stx::load_record{left, parse_dictionary_keys()}; }
    if (symbol_ == symbols.set) { return stx::load_top{left, right}; }
    if (symbol_ == symbols.list) { return stx::load_top{left, right}; }
    if (symbol_ == symbols.subscript) {
      auto [variable, field_] = parse_variable_and_field_from_subscript();
      return stx::load_field{left, variable, field_};
    }
    unreachable();
  }

  if (symbol_ == symbols.subscript) {
    auto [variable, field_] = parse_variable_and_field_from_subscript();
    goto_parent();
    goto_sibling();  // skip "="
    goto_sibling();
    stanly_assert(symbol() == symbols.identifier);
    return stx::set_field{text(node()), variable, field_};
  };

  unreachable();
}
}  // namespace parser
template std::vector<stx::node> parse<stx>(string_view);
}  // namespace stanly
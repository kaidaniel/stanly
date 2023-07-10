
#pragma once

#include <string_view>

#include "stanly-assert.h"
#include "tree_sitter/api.h"

extern "C" {
TSLanguage*
tree_sitter_python(void);
}

namespace stanly {

TSSymbol
lookup_symbol(std::string_view name);
TSFieldId
lookup_field(std::string_view name);

struct fields {
  TSFieldId left = lookup_field("left");
  TSFieldId right = lookup_field("right");
  TSFieldId key = lookup_field("key");
  TSFieldId value = lookup_field("value");
  TSFieldId subscript = lookup_field("subscript");
} const fields{};
enum class simple_statement {
  import_statement = 108,
  future_import_statement = 111,
  import_from_statement = 112,
  print_statement = 116,
  assert_statement = 118,
  expression_statement = 119,
  return_statement = 122,
  delete_statement = 123,
  raise_statement = 124,
  pass_statement = 125,
  break_statement = 126,
  continue_statement = 127,
  global_statement = 146,
  nonlocal_statement = 147,
  exec_statement = 148,
};
enum class compound_statement {
  if_statement = 128,
  match_statement = 131,
  for_statement = 133,
  while_statement = 134,
  try_statement = 135,
  with_statement = 138,
  function_definition = 141,
  class_definition = 149,
  decorated_definition = 152,
};
enum class symbs {
  identifier = 1,
  integer = 92,
  module = 105,
  assignment = 178,
  subscript = 184,
  list = 190,
  set = 191,
  dictionary = 193,
  pair = 194,
  string = 206,
};
// clang-format off
inline void
check_symbols() {
  stanly_assert(static_cast<TSSymbol>(simple_statement::import_statement) == lookup_symbol("import_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::future_import_statement) == lookup_symbol("future_import_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::import_from_statement) == lookup_symbol("import_from_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::print_statement) == lookup_symbol("print_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::assert_statement) == lookup_symbol("assert_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::expression_statement) == lookup_symbol("expression_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::return_statement) == lookup_symbol("return_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::delete_statement) == lookup_symbol("delete_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::raise_statement) == lookup_symbol("raise_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::pass_statement) == lookup_symbol("pass_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::break_statement) == lookup_symbol("break_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::continue_statement) == lookup_symbol("continue_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::global_statement) == lookup_symbol("global_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::nonlocal_statement) == lookup_symbol("nonlocal_statement"));
  stanly_assert(static_cast<TSSymbol>(simple_statement::exec_statement) == lookup_symbol("exec_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::if_statement) == lookup_symbol("if_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::match_statement) == lookup_symbol("match_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::for_statement) == lookup_symbol("for_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::while_statement) == lookup_symbol("while_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::try_statement) == lookup_symbol("try_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::with_statement) == lookup_symbol("with_statement"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::function_definition) == lookup_symbol("function_definition"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::class_definition) == lookup_symbol("class_definition"));
  stanly_assert(static_cast<TSSymbol>(compound_statement::decorated_definition) == lookup_symbol("decorated_definition"));
  stanly_assert(static_cast<TSSymbol>(symbs::identifier) == lookup_symbol("identifier"));
  stanly_assert(static_cast<TSSymbol>(symbs::integer) == lookup_symbol("integer"));
  stanly_assert(static_cast<TSSymbol>(symbs::module) == lookup_symbol("module"));
  stanly_assert(static_cast<TSSymbol>(symbs::assignment) == lookup_symbol("assignment"));
  stanly_assert(static_cast<TSSymbol>(symbs::subscript) == lookup_symbol("subscript"));
  stanly_assert(static_cast<TSSymbol>(symbs::list) == lookup_symbol("list"));
  stanly_assert(static_cast<TSSymbol>(symbs::set) == lookup_symbol("set"));
  stanly_assert(static_cast<TSSymbol>(symbs::dictionary) == lookup_symbol("dictionary"));
  stanly_assert(static_cast<TSSymbol>(symbs::pair) == lookup_symbol("pair"));
  stanly_assert(static_cast<TSSymbol>(symbs::string) == lookup_symbol("string"));
};
// clang-format on

std::string
generate_tree_sitter_symbols();
}  // namespace stanly

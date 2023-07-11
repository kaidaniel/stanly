#include "symbol-tables.h"

#include <map>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace stanly {
std::string
generate_tree_sitter_symbols() {
  auto kw = [](std::string s) {
    static std::set<std::string> cpp_key_words{"true", "false", "int", "float"};
    return cpp_key_words.contains(s) ? std::format("s_{}", s) : s;
  };

  std::string check = "// clang-format off\ninline void\ncheck_symbols() {\n";
  auto make_enum = [&](std::string_view name,
                       const std::vector<std::string>& items) -> std::string {
    std::map<TSSymbol, std::tuple<std::string, std::string>> m;
    std::string out = std::format("enum class {} {}\n", name, "{");
    for (const auto& s : items) { m[lookup_symbol(s)] = std::tuple{kw(s), s}; };
    for (const auto& [sym, nm] : m) {
      out += std::format("  {} = {},\n", std::get<0>(nm), sym);
      check +=
          std::format("  stanly_assert(static_cast<TSSymbol>({}::{}) == lookup_symbol(\"{}\"));\n",
                      name, std::get<0>(nm), std::get<1>(nm));
    }
    return out + "};\n";
  };
  return
      R"(#pragma once

#include <string_view>

#include "stanly-assert.h"
#include "tree_sitter/api.h"

// generated by stanly::generate_tree_sitter_symbols()

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
  TSFieldId alias = lookup_field("alias");
  TSFieldId children = lookup_field("children");
  TSFieldId argument = lookup_field("argument");
} const fields{};

)" +
      make_enum(
          "simple_statement",
          {"assert_statement", "break_statement", "continue_statement", "delete_statement",
           "exec_statement", "expression_statement", "future_import_statement", "global_statement",
           "import_from_statement", "import_statement", "nonlocal_statement", "pass_statement",
           "print_statement", "raise_statement", "return_statement"}) +
      make_enum("compound_statement", {"class_definition", "decorated_definition", "for_statement",
                                       "function_definition", "if_statement", "match_statement",
                                       "try_statement", "while_statement", "with_statement"}) +
      make_enum("expression_statement",
                {"assignment", "augmented_assignment", "expression", "yield"}) +
      make_enum("expression", {"as_pattern",
                               "await",
                               "boolean_operator",
                               "comparison_operator",
                               "conditional_expression",
                               "lambda",
                               "named_expression",
                               "not_operator",
                               /*primary_expression*/ "attribute",
                               "binary_operator",
                               "call",
                               "concatenated_string",
                               "dictionary",
                               "dictionary_comprehension",
                               "ellipsis",
                               "false",
                               "float",
                               "generator_expression",
                               "identifier",
                               "integer",
                               "list",
                               "list_comprehension",
                               "none",
                               "parenthesized_expression",
                               "set",
                               "set_comprehension",
                               "string",
                               "subscript",
                               "true",
                               "tuple",
                               "unary_operator"}) +
      make_enum("symbs", {"assignment", "module", "identifier", "integer", "string", "dictionary",
                          "pair", "list", "set", "subscript"}) +
      check + "};\n// clang-format on\n" +
      R"(
std::string
generate_tree_sitter_symbols();
}  // namespace stanly
      )";
}

TSSymbol
lookup_symbol(std::string_view name) {
  return ts_language_symbol_for_name(tree_sitter_python(), name.data(), name.length(), true);
}
TSFieldId
lookup_field(std::string_view name) {
  return ts_language_field_id_for_name(tree_sitter_python(), name.data(), name.size());
}

}  // namespace stanly
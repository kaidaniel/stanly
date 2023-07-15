#pragma once
#include <string_view>
#include <utility>

// generated using "./generate_parser_skeleton.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly::parser {

enum class fields;
struct parser;

std::optional<std::string_view> parse_field(parser&, enum fields);
std::optional<std::string_view> parse_children(parser&);

enum class symbols {
  sym_identifier = 1,
  sym_ellipsis = 83,
  sym_escape_sequence = 88,
  sym_type_conversion = 91,
  sym_integer = 92,
  sym_float = 93,
  sym_true = 95,
  sym_false = 96,
  sym_none = 97,
  sym_comment = 98,
  sym_module = 105,
  sym_import_statement = 108,
  sym_import_prefix = 109,
  sym_relative_import = 110,
  sym_future_import_statement = 111,
  sym_import_from_statement = 112,
  sym_aliased_import = 114,
  sym_wildcard_import = 115,
  sym_print_statement = 116,
  sym_chevron = 117,
  sym_assert_statement = 118,
  sym_expression_statement = 119,
  sym_named_expression = 120,
  sym_return_statement = 122,
  sym_delete_statement = 123,
  sym_raise_statement = 124,
  sym_pass_statement = 125,
  sym_break_statement = 126,
  sym_continue_statement = 127,
  sym_if_statement = 128,
  sym_elif_clause = 129,
  sym_else_clause = 130,
  sym_match_statement = 131,
  sym_case_clause = 132,
  sym_for_statement = 133,
  sym_while_statement = 134,
  sym_try_statement = 135,
  sym_except_clause = 136,
  sym_finally_clause = 137,
  sym_with_statement = 138,
  sym_with_clause = 139,
  sym_with_item = 140,
  sym_function_definition = 141,
  sym_parameters = 142,
  sym_lambda_parameters = 143,
  sym_list_splat = 144,
  sym_dictionary_splat = 145,
  sym_global_statement = 146,
  sym_nonlocal_statement = 147,
  sym_exec_statement = 148,
  sym_class_definition = 149,
  sym_parenthesized_list_splat = 150,
  sym_argument_list = 151,
  sym_decorated_definition = 152,
  sym_decorator = 153,
  sym_block = 154,
  sym_expression_list = 155,
  sym_dotted_name = 156,
  sym_parameter = 159,
  sym_pattern = 160,
  sym_tuple_pattern = 161,
  sym_list_pattern = 162,
  sym_default_parameter = 163,
  sym_typed_default_parameter = 164,
  sym_list_splat_pattern = 165,
  sym_dictionary_splat_pattern = 166,
  sym_as_pattern = 167,
  sym_expression = 169,
  sym_primary_expression = 170,
  sym_not_operator = 171,
  sym_boolean_operator = 172,
  sym_binary_operator = 173,
  sym_unary_operator = 174,
  sym_comparison_operator = 175,
  sym_lambda = 176,
  sym_assignment = 178,
  sym_augmented_assignment = 179,
  sym_pattern_list = 180,
  sym_yield = 182,
  sym_attribute = 183,
  sym_subscript = 184,
  sym_slice = 185,
  sym_call = 186,
  sym_typed_parameter = 187,
  sym_type = 188,
  sym_keyword_argument = 189,
  sym_list = 190,
  sym_set = 191,
  sym_tuple = 192,
  sym_dictionary = 193,
  sym_pair = 194,
  sym_list_comprehension = 195,
  sym_dictionary_comprehension = 196,
  sym_set_comprehension = 197,
  sym_generator_expression = 198,
  sym_parenthesized_expression = 200,
  sym_for_in_clause = 202,
  sym_if_clause = 203,
  sym_conditional_expression = 204,
  sym_concatenated_string = 205,
  sym_string = 206,
  sym_interpolation = 207,
  sym_format_specifier = 209,
  sym_format_expression = 210,
  sym_await = 211,
  sym_positional_separator = 212,
  sym_keyword_separator = 213,
  sym_case_pattern = 242,
};

enum class fields {
  sym_alias = 1,
  sym_alternative = 2,
  sym_argument = 3,
  sym_arguments = 4,
  sym_attribute = 5,
  sym_body = 6,
  sym_cause = 7,
  sym_code = 8,
  sym_condition = 9,
  sym_consequence = 10,
  sym_definition = 11,
  sym_function = 12,
  sym_guard = 13,
  sym_key = 14,
  sym_left = 15,
  sym_module_name = 16,
  sym_name = 17,
  sym_object = 18,
  sym_operator = 19,
  sym_operators = 20,
  sym_parameters = 21,
  sym_pattern = 22,
  sym_return_type = 23,
  sym_right = 24,
  sym_subject = 25,
  sym_subscript = 26,
  sym_superclasses = 27,
  sym_type = 28,
  sym_value = 29,
};

void parse__compound_statement(parser&);
void parse__simple_statement(parser&);
void parse_expression(parser&);
void parse_parameter(parser&);
void parse_pattern(parser&);
void parse_primary_expression(parser&);
void parse_aliased_import(parser&);
void parse_argument_list(parser&);
void parse_as_pattern(parser&);
void parse_assert_statement(parser&);
void parse_assignment(parser&);
void parse_attribute(parser&);
void parse_augmented_assignment(parser&);
void parse_await(parser&);
void parse_binary_operator(parser&);
void parse_block(parser&);
void parse_boolean_operator(parser&);
void parse_break_statement(parser&);
void parse_call(parser&);
void parse_case_clause(parser&);
void parse_case_pattern(parser&);
void parse_chevron(parser&);
void parse_class_definition(parser&);
void parse_comparison_operator(parser&);
void parse_concatenated_string(parser&);
void parse_conditional_expression(parser&);
void parse_continue_statement(parser&);
void parse_decorated_definition(parser&);
void parse_decorator(parser&);
void parse_default_parameter(parser&);
void parse_delete_statement(parser&);
void parse_dictionary(parser&);
void parse_dictionary_comprehension(parser&);
void parse_dictionary_splat(parser&);
void parse_dictionary_splat_pattern(parser&);
void parse_dotted_name(parser&);
void parse_elif_clause(parser&);
void parse_else_clause(parser&);
void parse_except_clause(parser&);
void parse_exec_statement(parser&);
void parse_expression_list(parser&);
void parse_expression_statement(parser&);
void parse_finally_clause(parser&);
void parse_for_in_clause(parser&);
void parse_for_statement(parser&);
void parse_format_expression(parser&);
void parse_format_specifier(parser&);
void parse_function_definition(parser&);
void parse_future_import_statement(parser&);
void parse_generator_expression(parser&);
void parse_global_statement(parser&);
void parse_if_clause(parser&);
void parse_if_statement(parser&);
void parse_import_from_statement(parser&);
void parse_import_prefix(parser&);
void parse_import_statement(parser&);
void parse_interpolation(parser&);
void parse_keyword_argument(parser&);
void parse_keyword_separator(parser&);
void parse_lambda(parser&);
void parse_lambda_parameters(parser&);
void parse_list(parser&);
void parse_list_comprehension(parser&);
void parse_list_pattern(parser&);
void parse_list_splat(parser&);
void parse_list_splat_pattern(parser&);
void parse_match_statement(parser&);
void parse_module(parser&);
void parse_named_expression(parser&);
void parse_nonlocal_statement(parser&);
void parse_not_operator(parser&);
void parse_pair(parser&);
void parse_parameters(parser&);
void parse_parenthesized_expression(parser&);
void parse_parenthesized_list_splat(parser&);
void parse_pass_statement(parser&);
void parse_pattern_list(parser&);
void parse_positional_separator(parser&);
void parse_print_statement(parser&);
void parse_raise_statement(parser&);
void parse_relative_import(parser&);
void parse_return_statement(parser&);
void parse_set(parser&);
void parse_set_comprehension(parser&);
void parse_slice(parser&);
void parse_string(parser&);
void parse_subscript(parser&);
void parse_try_statement(parser&);
void parse_tuple(parser&);
void parse_tuple_pattern(parser&);
void parse_type(parser&);
void parse_typed_default_parameter(parser&);
void parse_typed_parameter(parser&);
void parse_unary_operator(parser&);
void parse_while_statement(parser&);
void parse_wildcard_import(parser&);
void parse_with_clause(parser&);
void parse_with_item(parser&);
void parse_with_statement(parser&);
void parse_yield(parser&);
void parse_comment(parser&);
void parse_ellipsis(parser&);
void parse_escape_sequence(parser&);
void parse_false(parser&);
void parse_float(parser&);
void parse_identifier(parser&);
void parse_integer(parser&);
void parse_none(parser&);
void parse_true(parser&);
void parse_type_conversion(parser&);
}  // namespace stanly::parser

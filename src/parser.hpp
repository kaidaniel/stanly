#pragma once

// generated using "generate_parser_symbols.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly::parser {

enum class terminals;
enum class fields;
enum class symbols;
struct parser;

void
parse(parser&, terminals);
void
parse_field(parser&, fields, void (*)(parser&, symbols));
void
parse_all_children(parser&, void (*)(parser&, symbols));

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

enum class terminals {
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
  sym_import_prefix = 109,
  sym_wildcard_import = 115,
  sym_pass_statement = 125,
  sym_break_statement = 126,
  sym_continue_statement = 127,
  sym_positional_separator = 212,
  sym_keyword_separator = 213,
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

namespace sym__compound_statement {
void
parse(parser&, enum symbols);
enum class subtypes {
  sym_if_statement = 128,
  sym_match_statement = 131,
  sym_for_statement = 133,
  sym_while_statement = 134,
  sym_try_statement = 135,
  sym_with_statement = 138,
  sym_function_definition = 141,
  sym_class_definition = 149,
  sym_decorated_definition = 152,
};
namespace fields {

}
}  // namespace sym__compound_statement
namespace sym__simple_statement {
void
parse(parser&, enum symbols);
enum class subtypes {
  sym_import_statement = 108,
  sym_future_import_statement = 111,
  sym_import_from_statement = 112,
  sym_print_statement = 116,
  sym_assert_statement = 118,
  sym_expression_statement = 119,
  sym_return_statement = 122,
  sym_delete_statement = 123,
  sym_raise_statement = 124,
  sym_pass_statement = 125,
  sym_break_statement = 126,
  sym_continue_statement = 127,
  sym_global_statement = 146,
  sym_nonlocal_statement = 147,
  sym_exec_statement = 148,
};
namespace fields {

}
}  // namespace sym__simple_statement
namespace sym_expression {
void
parse(parser&, enum symbols);
enum class subtypes {
  sym_named_expression = 120,
  sym_as_pattern = 167,
  sym_primary_expression = 170,
  sym_not_operator = 171,
  sym_boolean_operator = 172,
  sym_comparison_operator = 175,
  sym_lambda = 176,
  sym_conditional_expression = 204,
  sym_await = 211,
};
namespace fields {

}
}  // namespace sym_expression
namespace sym_parameter {
void
parse(parser&, enum symbols);
enum class subtypes {
  sym_identifier = 1,
  sym_tuple_pattern = 161,
  sym_default_parameter = 163,
  sym_typed_default_parameter = 164,
  sym_list_splat_pattern = 165,
  sym_dictionary_splat_pattern = 166,
  sym_typed_parameter = 187,
  sym_positional_separator = 212,
  sym_keyword_separator = 213,
};
namespace fields {

}
}  // namespace sym_parameter
namespace sym_pattern {
void
parse(parser&, enum symbols);
enum class subtypes {
  sym_identifier = 1,
  sym_tuple_pattern = 161,
  sym_list_pattern = 162,
  sym_list_splat_pattern = 165,
  sym_attribute = 183,
  sym_subscript = 184,
};
namespace fields {

}
}  // namespace sym_pattern
namespace sym_primary_expression {
void
parse(parser&, enum symbols);
enum class subtypes {
  sym_identifier = 1,
  sym_ellipsis = 83,
  sym_integer = 92,
  sym_float = 93,
  sym_true = 95,
  sym_false = 96,
  sym_none = 97,
  sym_binary_operator = 173,
  sym_unary_operator = 174,
  sym_attribute = 183,
  sym_subscript = 184,
  sym_call = 186,
  sym_list = 190,
  sym_set = 191,
  sym_tuple = 192,
  sym_dictionary = 193,
  sym_list_comprehension = 195,
  sym_dictionary_comprehension = 196,
  sym_set_comprehension = 197,
  sym_generator_expression = 198,
  sym_parenthesized_expression = 200,
  sym_concatenated_string = 205,
  sym_string = 206,
};
namespace fields {

}
}  // namespace sym_primary_expression
namespace sym_aliased_import {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_alias = 1,
  sym_name = 17,
};
enum class alias {
  sym_identifier = 1,
};
enum class name {
  sym_dotted_name = 156,
};
}  // namespace fields
}  // namespace sym_aliased_import
namespace sym_argument_list {
void
parse(parser&, enum symbols);
enum class children {
  sym_list_splat = 144,
  sym_dictionary_splat = 145,
  sym_expression = 169,
  sym_keyword_argument = 189,
  sym_parenthesized_expression = 200,
};
namespace fields {

}
}  // namespace sym_argument_list
namespace sym_as_pattern {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {
enum class field_names {
  sym_alias = 1,
};
enum class alias {
  sym_as_pattern_target = 241,
};
}  // namespace fields
}  // namespace sym_as_pattern
namespace sym_assert_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_assert_statement
namespace sym_assignment {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_left = 15,
  sym_right = 24,
  sym_type = 28,
};
enum class left {
  sym_pattern = 160,
  sym_pattern_list = 180,
};
enum class right {
  sym_expression_list = 155,
  sym_expression = 169,
  sym_assignment = 178,
  sym_augmented_assignment = 179,
  sym_yield = 182,
};
enum class type {
  sym_type = 188,
};
}  // namespace fields
}  // namespace sym_assignment
namespace sym_attribute {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_attribute = 5,
  sym_object = 18,
};
enum class attribute {
  sym_identifier = 1,
};
enum class object {
  sym_primary_expression = 170,
};
}  // namespace fields
}  // namespace sym_attribute
namespace sym_augmented_assignment {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_left = 15,
  sym_operator = 19,
  sym_right = 24,
};
enum class left {
  sym_pattern = 160,
  sym_pattern_list = 180,
};
enum class right {
  sym_expression_list = 155,
  sym_expression = 169,
  sym_assignment = 178,
  sym_augmented_assignment = 179,
  sym_yield = 182,
};
}  // namespace fields
}  // namespace sym_augmented_assignment
namespace sym_await {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_await
namespace sym_binary_operator {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_left = 15,
  sym_operator = 19,
  sym_right = 24,
};
enum class left {
  sym_primary_expression = 170,
};
enum class right {
  sym_primary_expression = 170,
};
}  // namespace fields
}  // namespace sym_binary_operator
namespace sym_block {
void
parse(parser&, enum symbols);
namespace fields {

}
}  // namespace sym_block
namespace sym_boolean_operator {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_left = 15,
  sym_operator = 19,
  sym_right = 24,
};
enum class left {
  sym_expression = 169,
};
enum class right {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_boolean_operator
namespace sym_call {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_arguments = 4,
  sym_function = 12,
};
enum class arguments {
  sym_argument_list = 151,
  sym_generator_expression = 198,
};
enum class function {
  sym_primary_expression = 170,
};
}  // namespace fields
}  // namespace sym_call
namespace sym_case_clause {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_consequence = 10,
  sym_guard = 13,
  sym_pattern = 22,
};
enum class consequence {
  sym_block = 154,
};
enum class guard {
  sym_if_clause = 203,
};
enum class pattern {
  sym_case_pattern = 242,
};
}  // namespace fields
}  // namespace sym_case_clause
namespace sym_case_pattern {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
  sym_attribute = 183,
  sym_subscript = 184,
};
namespace fields {

}
}  // namespace sym_case_pattern
namespace sym_chevron {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_chevron
namespace sym_class_definition {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_body = 6,
  sym_name = 17,
  sym_superclasses = 27,
};
enum class body {
  sym_block = 154,
};
enum class name {
  sym_identifier = 1,
};
enum class superclasses {
  sym_argument_list = 151,
};
}  // namespace fields
}  // namespace sym_class_definition
namespace sym_comparison_operator {
void
parse(parser&, enum symbols);
enum class children {
  sym_primary_expression = 170,
};
namespace fields {
enum class field_names {
  sym_operators = 20,
};
}  // namespace fields
}  // namespace sym_comparison_operator
namespace sym_concatenated_string {
void
parse(parser&, enum symbols);
enum class children {
  sym_string = 206,
};
namespace fields {

}
}  // namespace sym_concatenated_string
namespace sym_conditional_expression {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_conditional_expression
namespace sym_decorated_definition {
void
parse(parser&, enum symbols);
enum class children {
  sym_decorator = 153,
};
namespace fields {
enum class field_names {
  sym_definition = 11,
};
enum class definition {
  sym_function_definition = 141,
  sym_class_definition = 149,
};
}  // namespace fields
}  // namespace sym_decorated_definition
namespace sym_decorator {
void
parse(parser&, enum symbols);
enum class children {
  sym_primary_expression = 170,
};
namespace fields {

}
}  // namespace sym_decorator
namespace sym_default_parameter {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_name = 17,
  sym_value = 29,
};
enum class name {
  sym_identifier = 1,
};
enum class value {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_default_parameter
namespace sym_delete_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression_list = 155,
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_delete_statement
namespace sym_dictionary {
void
parse(parser&, enum symbols);
enum class children {
  sym_dictionary_splat = 145,
  sym_pair = 194,
};
namespace fields {

}
}  // namespace sym_dictionary
namespace sym_dictionary_comprehension {
void
parse(parser&, enum symbols);
enum class children {
  sym_for_in_clause = 202,
  sym_if_clause = 203,
};
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_pair = 194,
};
}  // namespace fields
}  // namespace sym_dictionary_comprehension
namespace sym_dictionary_splat {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_dictionary_splat
namespace sym_dictionary_splat_pattern {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
  sym_attribute = 183,
  sym_subscript = 184,
};
namespace fields {

}
}  // namespace sym_dictionary_splat_pattern
namespace sym_dotted_name {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
};
namespace fields {

}
}  // namespace sym_dotted_name
namespace sym_elif_clause {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_condition = 9,
  sym_consequence = 10,
};
enum class condition {
  sym_expression = 169,
};
enum class consequence {
  sym_block = 154,
};
}  // namespace fields
}  // namespace sym_elif_clause
namespace sym_else_clause {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_block = 154,
};
}  // namespace fields
}  // namespace sym_else_clause
namespace sym_except_clause {
void
parse(parser&, enum symbols);
enum class children {
  sym_block = 154,
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_except_clause
namespace sym_exec_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {
enum class field_names {
  sym_code = 8,
};
enum class code {
  sym_string = 206,
};
}  // namespace fields
}  // namespace sym_exec_statement
namespace sym_expression_list {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_expression_list
namespace sym_expression_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
  sym_assignment = 178,
  sym_augmented_assignment = 179,
  sym_yield = 182,
};
namespace fields {

}
}  // namespace sym_expression_statement
namespace sym_finally_clause {
void
parse(parser&, enum symbols);
enum class children {
  sym_block = 154,
};
namespace fields {

}
}  // namespace sym_finally_clause
namespace sym_for_in_clause {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_left = 15,
  sym_right = 24,
};
enum class left {
  sym_pattern = 160,
  sym_pattern_list = 180,
};
enum class right {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_for_in_clause
namespace sym_for_statement {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_alternative = 2,
  sym_body = 6,
  sym_left = 15,
  sym_right = 24,
};
enum class alternative {
  sym_else_clause = 130,
};
enum class body {
  sym_block = 154,
};
enum class left {
  sym_pattern = 160,
  sym_pattern_list = 180,
};
enum class right {
  sym_expression_list = 155,
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_for_statement
namespace sym_format_expression {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_format_expression
namespace sym_format_specifier {
void
parse(parser&, enum symbols);
enum class children {
  sym_format_expression = 210,
};
namespace fields {

}
}  // namespace sym_format_specifier
namespace sym_function_definition {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_body = 6,
  sym_name = 17,
  sym_parameters = 21,
  sym_return_type = 23,
};
enum class body {
  sym_block = 154,
};
enum class name {
  sym_identifier = 1,
};
enum class parameters {
  sym_parameters = 142,
};
enum class return_type {
  sym_type = 188,
};
}  // namespace fields
}  // namespace sym_function_definition
namespace sym_future_import_statement {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_name = 17,
};
enum class name {
  sym_aliased_import = 114,
  sym_dotted_name = 156,
};
}  // namespace fields
}  // namespace sym_future_import_statement
namespace sym_generator_expression {
void
parse(parser&, enum symbols);
enum class children {
  sym_for_in_clause = 202,
  sym_if_clause = 203,
};
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_generator_expression
namespace sym_global_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
};
namespace fields {

}
}  // namespace sym_global_statement
namespace sym_if_clause {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_if_clause
namespace sym_if_statement {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_alternative = 2,
  sym_condition = 9,
  sym_consequence = 10,
};
enum class alternative {
  sym_elif_clause = 129,
  sym_else_clause = 130,
};
enum class condition {
  sym_expression = 169,
};
enum class consequence {
  sym_block = 154,
};
}  // namespace fields
}  // namespace sym_if_statement
namespace sym_import_from_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_wildcard_import = 115,
};
namespace fields {
enum class field_names {
  sym_module_name = 16,
  sym_name = 17,
};
enum class module_name {
  sym_relative_import = 110,
  sym_dotted_name = 156,
};
enum class name {
  sym_aliased_import = 114,
  sym_dotted_name = 156,
};
}  // namespace fields
}  // namespace sym_import_from_statement
namespace sym_import_statement {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_name = 17,
};
enum class name {
  sym_aliased_import = 114,
  sym_dotted_name = 156,
};
}  // namespace fields
}  // namespace sym_import_statement
namespace sym_interpolation {
void
parse(parser&, enum symbols);
enum class children {
  sym_type_conversion = 91,
  sym_expression = 169,
  sym_format_specifier = 209,
};
namespace fields {

}
}  // namespace sym_interpolation
namespace sym_keyword_argument {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_name = 17,
  sym_value = 29,
};
enum class name {
  sym_identifier = 1,
};
enum class value {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_keyword_argument
namespace sym_lambda {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_body = 6,
  sym_parameters = 21,
};
enum class body {
  sym_expression = 169,
};
enum class parameters {
  sym_lambda_parameters = 143,
};
}  // namespace fields
}  // namespace sym_lambda
namespace sym_lambda_parameters {
void
parse(parser&, enum symbols);
enum class children {
  sym_parameter = 159,
};
namespace fields {

}
}  // namespace sym_lambda_parameters
namespace sym_list {
void
parse(parser&, enum symbols);
enum class children {
  sym_list_splat = 144,
  sym_parenthesized_list_splat = 150,
  sym_expression = 169,
  sym_yield = 182,
};
namespace fields {

}
}  // namespace sym_list
namespace sym_list_comprehension {
void
parse(parser&, enum symbols);
enum class children {
  sym_for_in_clause = 202,
  sym_if_clause = 203,
};
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_list_comprehension
namespace sym_list_pattern {
void
parse(parser&, enum symbols);
enum class children {
  sym_pattern = 160,
};
namespace fields {

}
}  // namespace sym_list_pattern
namespace sym_list_splat {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_list_splat
namespace sym_list_splat_pattern {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
  sym_attribute = 183,
  sym_subscript = 184,
};
namespace fields {

}
}  // namespace sym_list_splat_pattern
namespace sym_match_statement {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_alternative = 2,
  sym_subject = 25,
};
enum class alternative {
  sym_case_clause = 132,
};
enum class subject {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_match_statement
namespace sym_module {
void
parse(parser&, enum symbols);
namespace fields {

}
}  // namespace sym_module
namespace sym_named_expression {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_name = 17,
  sym_value = 29,
};
enum class name {
  sym_identifier = 1,
};
enum class value {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_named_expression
namespace sym_nonlocal_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
};
namespace fields {

}
}  // namespace sym_nonlocal_statement
namespace sym_not_operator {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_argument = 3,
};
enum class argument {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_not_operator
namespace sym_pair {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_key = 14,
  sym_value = 29,
};
enum class key {
  sym_expression = 169,
};
enum class value {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_pair
namespace sym_parameters {
void
parse(parser&, enum symbols);
enum class children {
  sym_parameter = 159,
};
namespace fields {

}
}  // namespace sym_parameters
namespace sym_parenthesized_expression {
void
parse(parser&, enum symbols);
enum class children {
  sym_list_splat = 144,
  sym_expression = 169,
  sym_yield = 182,
  sym_parenthesized_expression = 200,
};
namespace fields {

}
}  // namespace sym_parenthesized_expression
namespace sym_parenthesized_list_splat {
void
parse(parser&, enum symbols);
enum class children {
  sym_list_splat = 144,
  sym_parenthesized_expression = 200,
};
namespace fields {

}
}  // namespace sym_parenthesized_list_splat
namespace sym_pattern_list {
void
parse(parser&, enum symbols);
enum class children {
  sym_pattern = 160,
};
namespace fields {

}
}  // namespace sym_pattern_list
namespace sym_print_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_chevron = 117,
};
namespace fields {
enum class field_names {
  sym_argument = 3,
};
enum class argument {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_print_statement
namespace sym_raise_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression_list = 155,
  sym_expression = 169,
};
namespace fields {
enum class field_names {
  sym_cause = 7,
};
enum class cause {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_raise_statement
namespace sym_relative_import {
void
parse(parser&, enum symbols);
enum class children {
  sym_import_prefix = 109,
  sym_dotted_name = 156,
};
namespace fields {

}
}  // namespace sym_relative_import
namespace sym_return_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression_list = 155,
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_return_statement
namespace sym_set {
void
parse(parser&, enum symbols);
enum class children {
  sym_list_splat = 144,
  sym_parenthesized_list_splat = 150,
  sym_expression = 169,
  sym_yield = 182,
};
namespace fields {

}
}  // namespace sym_set
namespace sym_set_comprehension {
void
parse(parser&, enum symbols);
enum class children {
  sym_for_in_clause = 202,
  sym_if_clause = 203,
};
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_set_comprehension
namespace sym_slice {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_slice
namespace sym_string {
void
parse(parser&, enum symbols);
enum class children {
  sym_escape_sequence = 88,
  sym_interpolation = 207,
};
namespace fields {

}
}  // namespace sym_string
namespace sym_subscript {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_subscript = 26,
  sym_value = 29,
};
enum class subscript {
  sym_expression = 169,
  sym_slice = 185,
};
enum class value {
  sym_primary_expression = 170,
};
}  // namespace fields
}  // namespace sym_subscript
namespace sym_try_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_else_clause = 130,
  sym_except_clause = 136,
  sym_finally_clause = 137,
};
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_block = 154,
};
}  // namespace fields
}  // namespace sym_try_statement
namespace sym_tuple {
void
parse(parser&, enum symbols);
enum class children {
  sym_list_splat = 144,
  sym_parenthesized_list_splat = 150,
  sym_expression = 169,
  sym_yield = 182,
};
namespace fields {

}
}  // namespace sym_tuple
namespace sym_tuple_pattern {
void
parse(parser&, enum symbols);
enum class children {
  sym_pattern = 160,
};
namespace fields {

}
}  // namespace sym_tuple_pattern
namespace sym_type {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_type
namespace sym_typed_default_parameter {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_name = 17,
  sym_type = 28,
  sym_value = 29,
};
enum class name {
  sym_identifier = 1,
};
enum class type {
  sym_type = 188,
};
enum class value {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_typed_default_parameter
namespace sym_typed_parameter {
void
parse(parser&, enum symbols);
enum class children {
  sym_identifier = 1,
  sym_list_splat_pattern = 165,
  sym_dictionary_splat_pattern = 166,
};
namespace fields {
enum class field_names {
  sym_type = 28,
};
enum class type {
  sym_type = 188,
};
}  // namespace fields
}  // namespace sym_typed_parameter
namespace sym_unary_operator {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_argument = 3,
  sym_operator = 19,
};
enum class argument {
  sym_primary_expression = 170,
};
}  // namespace fields
}  // namespace sym_unary_operator
namespace sym_while_statement {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_alternative = 2,
  sym_body = 6,
  sym_condition = 9,
};
enum class alternative {
  sym_else_clause = 130,
};
enum class body {
  sym_block = 154,
};
enum class condition {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_while_statement
namespace sym_with_clause {
void
parse(parser&, enum symbols);
enum class children {
  sym_with_item = 140,
};
namespace fields {

}
}  // namespace sym_with_clause
namespace sym_with_item {
void
parse(parser&, enum symbols);
namespace fields {
enum class field_names {
  sym_value = 29,
};
enum class value {
  sym_expression = 169,
};
}  // namespace fields
}  // namespace sym_with_item
namespace sym_with_statement {
void
parse(parser&, enum symbols);
enum class children {
  sym_with_clause = 139,
};
namespace fields {
enum class field_names {
  sym_body = 6,
};
enum class body {
  sym_block = 154,
};
}  // namespace fields
}  // namespace sym_with_statement
namespace sym_yield {
void
parse(parser&, enum symbols);
enum class children {
  sym_expression_list = 155,
  sym_expression = 169,
};
namespace fields {

}
}  // namespace sym_yield
}  // namespace stanly::parser

#pragma once

// generated by "./generate-parser-symbols.sh build-default/tree-sitter-python/src/node-types.json "

namespace stanly {

enum class symbol {
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
  sym_tuple_pattern = 161,
  sym_list_pattern = 162,
  sym_default_parameter = 163,
  sym_typed_default_parameter = 164,
  sym_list_splat_pattern = 165,
  sym_dictionary_splat_pattern = 166,
  sym_as_pattern = 167,
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
enum class field {
  fld_alias = 1,
  fld_alternative = 2,
  fld_argument = 3,
  fld_arguments = 4,
  fld_attribute = 5,
  fld_body = 6,
  fld_cause = 7,
  fld_code = 8,
  fld_condition = 9,
  fld_consequence = 10,
  fld_definition = 11,
  fld_function = 12,
  fld_guard = 13,
  fld_key = 14,
  fld_left = 15,
  fld_module_name = 16,
  fld_name = 17,
  fld_object = 18,
  fld_operator = 19,
  fld_operators = 20,
  fld_parameters = 21,
  fld_pattern = 22,
  fld_return_type = 23,
  fld_right = 24,
  fld_subject = 25,
  fld_subscript = 26,
  fld_superclasses = 27,
  fld_type = 28,
  fld_value = 29,
};

struct sym_aliased_import {
  enum class fields {
    fld_alias = static_cast<int>(field::fld_alias),
    fld_name = static_cast<int>(field::fld_name),
  };
};
struct sym_argument_list {
  enum class children {
    sym_dictionary_splat = static_cast<int>(symbol::sym_dictionary_splat),
    sym_keyword_argument = static_cast<int>(symbol::sym_keyword_argument),
    sym_list_splat = static_cast<int>(symbol::sym_list_splat),
    sym_parenthesized_expression = static_cast<int>(symbol::sym_parenthesized_expression),
  };
};
struct sym_as_pattern {
  enum class children {};
  enum class fields {
    fld_alias = static_cast<int>(field::fld_alias),
  };
};
struct sym_assert_statement {
  enum class children {};
};
struct sym_assignment {
  enum class fields {
    fld_left = static_cast<int>(field::fld_left),
    fld_right = static_cast<int>(field::fld_right),
    fld_type = static_cast<int>(field::fld_type),
  };
};
struct sym_attribute {
  enum class fields {
    fld_attribute = static_cast<int>(field::fld_attribute),
    fld_object = static_cast<int>(field::fld_object),
  };
};
struct sym_augmented_assignment {
  enum class fields {
    fld_left = static_cast<int>(field::fld_left),
    fld_operator = static_cast<int>(field::fld_operator),
    fld_right = static_cast<int>(field::fld_right),
  };
};
struct sym_await {
  enum class children {};
};
struct sym_binary_operator {
  enum class fields {
    fld_left = static_cast<int>(field::fld_left),
    fld_operator = static_cast<int>(field::fld_operator),
    fld_right = static_cast<int>(field::fld_right),
  };
};
struct sym_block {
  enum class children {};
};
struct sym_boolean_operator {
  enum class fields {
    fld_left = static_cast<int>(field::fld_left),
    fld_operator = static_cast<int>(field::fld_operator),
    fld_right = static_cast<int>(field::fld_right),
  };
};
struct sym_break_statement {};
struct sym_call {
  enum class fields {
    fld_arguments = static_cast<int>(field::fld_arguments),
    fld_function = static_cast<int>(field::fld_function),
  };
};
struct sym_case_clause {
  enum class fields {
    fld_consequence = static_cast<int>(field::fld_consequence),
    fld_guard = static_cast<int>(field::fld_guard),
    fld_pattern = static_cast<int>(field::fld_pattern),
  };
};
struct sym_case_pattern {
  enum class children {
    sym_attribute = static_cast<int>(symbol::sym_attribute),
    sym_identifier = static_cast<int>(symbol::sym_identifier),
    sym_subscript = static_cast<int>(symbol::sym_subscript),
  };
};
struct sym_chevron {
  enum class children {};
};
struct sym_class_definition {
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
    fld_name = static_cast<int>(field::fld_name),
    fld_superclasses = static_cast<int>(field::fld_superclasses),
  };
};
struct sym_comparison_operator {
  enum class children {};
  enum class fields {
    fld_operators = static_cast<int>(field::fld_operators),
  };
};
struct sym_concatenated_string {
  enum class children {
    sym_string = static_cast<int>(symbol::sym_string),
  };
};
struct sym_conditional_expression {
  enum class children {};
};
struct sym_continue_statement {};
struct sym_decorated_definition {
  enum class children {
    sym_decorator = static_cast<int>(symbol::sym_decorator),
  };
  enum class fields {
    fld_definition = static_cast<int>(field::fld_definition),
  };
};
struct sym_decorator {
  enum class children {};
};
struct sym_default_parameter {
  enum class fields {
    fld_name = static_cast<int>(field::fld_name),
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_delete_statement {
  enum class children {
    sym_expression_list = static_cast<int>(symbol::sym_expression_list),
  };
};
struct sym_dictionary {
  enum class children {
    sym_dictionary_splat = static_cast<int>(symbol::sym_dictionary_splat),
    sym_pair = static_cast<int>(symbol::sym_pair),
  };
};
struct sym_dictionary_comprehension {
  enum class children {
    sym_for_in_clause = static_cast<int>(symbol::sym_for_in_clause),
    sym_if_clause = static_cast<int>(symbol::sym_if_clause),
  };
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_dictionary_splat {
  enum class children {};
};
struct sym_dictionary_splat_pattern {
  enum class children {
    sym_attribute = static_cast<int>(symbol::sym_attribute),
    sym_identifier = static_cast<int>(symbol::sym_identifier),
    sym_subscript = static_cast<int>(symbol::sym_subscript),
  };
};
struct sym_dotted_name {
  enum class children {
    sym_identifier = static_cast<int>(symbol::sym_identifier),
  };
};
struct sym_elif_clause {
  enum class fields {
    fld_condition = static_cast<int>(field::fld_condition),
    fld_consequence = static_cast<int>(field::fld_consequence),
  };
};
struct sym_else_clause {
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_except_clause {
  enum class children {
    sym_block = static_cast<int>(symbol::sym_block),
  };
};
struct sym_exec_statement {
  enum class children {};
  enum class fields {
    fld_code = static_cast<int>(field::fld_code),
  };
};
struct sym_expression_list {
  enum class children {};
};
struct sym_expression_statement {
  enum class children {
    sym_assignment = static_cast<int>(symbol::sym_assignment),
    sym_augmented_assignment = static_cast<int>(symbol::sym_augmented_assignment),
    sym_yield = static_cast<int>(symbol::sym_yield),
  };
};
struct sym_finally_clause {
  enum class children {
    sym_block = static_cast<int>(symbol::sym_block),
  };
};
struct sym_for_in_clause {
  enum class fields {
    fld_left = static_cast<int>(field::fld_left),
    fld_right = static_cast<int>(field::fld_right),
  };
};
struct sym_for_statement {
  enum class fields {
    fld_alternative = static_cast<int>(field::fld_alternative),
    fld_body = static_cast<int>(field::fld_body),
    fld_left = static_cast<int>(field::fld_left),
    fld_right = static_cast<int>(field::fld_right),
  };
};
struct sym_format_expression {
  enum class children {};
};
struct sym_format_specifier {
  enum class children {
    sym_format_expression = static_cast<int>(symbol::sym_format_expression),
  };
};
struct sym_function_definition {
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
    fld_name = static_cast<int>(field::fld_name),
    fld_parameters = static_cast<int>(field::fld_parameters),
    fld_return_type = static_cast<int>(field::fld_return_type),
  };
};
struct sym_future_import_statement {
  enum class fields {
    fld_name = static_cast<int>(field::fld_name),
  };
};
struct sym_generator_expression {
  enum class children {
    sym_for_in_clause = static_cast<int>(symbol::sym_for_in_clause),
    sym_if_clause = static_cast<int>(symbol::sym_if_clause),
  };
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_global_statement {
  enum class children {
    sym_identifier = static_cast<int>(symbol::sym_identifier),
  };
};
struct sym_if_clause {
  enum class children {};
};
struct sym_if_statement {
  enum class fields {
    fld_alternative = static_cast<int>(field::fld_alternative),
    fld_condition = static_cast<int>(field::fld_condition),
    fld_consequence = static_cast<int>(field::fld_consequence),
  };
};
struct sym_import_from_statement {
  enum class children {
    sym_wildcard_import = static_cast<int>(symbol::sym_wildcard_import),
  };
  enum class fields {
    fld_module_name = static_cast<int>(field::fld_module_name),
    fld_name = static_cast<int>(field::fld_name),
  };
};
struct sym_import_prefix {};
struct sym_import_statement {
  enum class fields {
    fld_name = static_cast<int>(field::fld_name),
  };
};
struct sym_interpolation {
  enum class children {
    sym_format_specifier = static_cast<int>(symbol::sym_format_specifier),
    sym_type_conversion = static_cast<int>(symbol::sym_type_conversion),
  };
};
struct sym_keyword_argument {
  enum class fields {
    fld_name = static_cast<int>(field::fld_name),
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_keyword_separator {};
struct sym_lambda {
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
    fld_parameters = static_cast<int>(field::fld_parameters),
  };
};
struct sym_lambda_parameters {
  enum class children {};
};
struct sym_list {
  enum class children {
    sym_list_splat = static_cast<int>(symbol::sym_list_splat),
    sym_parenthesized_list_splat = static_cast<int>(symbol::sym_parenthesized_list_splat),
    sym_yield = static_cast<int>(symbol::sym_yield),
  };
};
struct sym_list_comprehension {
  enum class children {
    sym_for_in_clause = static_cast<int>(symbol::sym_for_in_clause),
    sym_if_clause = static_cast<int>(symbol::sym_if_clause),
  };
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_list_pattern {
  enum class children {};
};
struct sym_list_splat {
  enum class children {};
};
struct sym_list_splat_pattern {
  enum class children {
    sym_attribute = static_cast<int>(symbol::sym_attribute),
    sym_identifier = static_cast<int>(symbol::sym_identifier),
    sym_subscript = static_cast<int>(symbol::sym_subscript),
  };
};
struct sym_match_statement {
  enum class fields {
    fld_alternative = static_cast<int>(field::fld_alternative),
    fld_subject = static_cast<int>(field::fld_subject),
  };
};
struct sym_module {
  enum class children {};
};
struct sym_named_expression {
  enum class fields {
    fld_name = static_cast<int>(field::fld_name),
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_nonlocal_statement {
  enum class children {
    sym_identifier = static_cast<int>(symbol::sym_identifier),
  };
};
struct sym_not_operator {
  enum class fields {
    fld_argument = static_cast<int>(field::fld_argument),
  };
};
struct sym_pair {
  enum class fields {
    fld_key = static_cast<int>(field::fld_key),
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_parameters {
  enum class children {};
};
struct sym_parenthesized_expression {
  enum class children {
    sym_list_splat = static_cast<int>(symbol::sym_list_splat),
    sym_parenthesized_expression = static_cast<int>(symbol::sym_parenthesized_expression),
    sym_yield = static_cast<int>(symbol::sym_yield),
  };
};
struct sym_parenthesized_list_splat {
  enum class children {
    sym_list_splat = static_cast<int>(symbol::sym_list_splat),
    sym_parenthesized_expression = static_cast<int>(symbol::sym_parenthesized_expression),
  };
};
struct sym_pass_statement {};
struct sym_pattern_list {
  enum class children {};
};
struct sym_positional_separator {};
struct sym_print_statement {
  enum class children {
    sym_chevron = static_cast<int>(symbol::sym_chevron),
  };
  enum class fields {
    fld_argument = static_cast<int>(field::fld_argument),
  };
};
struct sym_raise_statement {
  enum class children {
    sym_expression_list = static_cast<int>(symbol::sym_expression_list),
  };
  enum class fields {
    fld_cause = static_cast<int>(field::fld_cause),
  };
};
struct sym_relative_import {
  enum class children {
    sym_dotted_name = static_cast<int>(symbol::sym_dotted_name),
    sym_import_prefix = static_cast<int>(symbol::sym_import_prefix),
  };
};
struct sym_return_statement {
  enum class children {
    sym_expression_list = static_cast<int>(symbol::sym_expression_list),
  };
};
struct sym_set {
  enum class children {
    sym_list_splat = static_cast<int>(symbol::sym_list_splat),
    sym_parenthesized_list_splat = static_cast<int>(symbol::sym_parenthesized_list_splat),
    sym_yield = static_cast<int>(symbol::sym_yield),
  };
};
struct sym_set_comprehension {
  enum class children {
    sym_for_in_clause = static_cast<int>(symbol::sym_for_in_clause),
    sym_if_clause = static_cast<int>(symbol::sym_if_clause),
  };
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_slice {
  enum class children {};
};
struct sym_string {
  enum class children {
    sym_escape_sequence = static_cast<int>(symbol::sym_escape_sequence),
    sym_interpolation = static_cast<int>(symbol::sym_interpolation),
  };
};
struct sym_subscript {
  enum class fields {
    fld_subscript = static_cast<int>(field::fld_subscript),
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_try_statement {
  enum class children {
    sym_else_clause = static_cast<int>(symbol::sym_else_clause),
    sym_except_clause = static_cast<int>(symbol::sym_except_clause),
    sym_finally_clause = static_cast<int>(symbol::sym_finally_clause),
  };
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_tuple {
  enum class children {
    sym_list_splat = static_cast<int>(symbol::sym_list_splat),
    sym_parenthesized_list_splat = static_cast<int>(symbol::sym_parenthesized_list_splat),
    sym_yield = static_cast<int>(symbol::sym_yield),
  };
};
struct sym_tuple_pattern {
  enum class children {};
};
struct sym_type {
  enum class children {};
};
struct sym_typed_default_parameter {
  enum class fields {
    fld_name = static_cast<int>(field::fld_name),
    fld_type = static_cast<int>(field::fld_type),
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_typed_parameter {
  enum class children {
    sym_dictionary_splat_pattern = static_cast<int>(symbol::sym_dictionary_splat_pattern),
    sym_identifier = static_cast<int>(symbol::sym_identifier),
    sym_list_splat_pattern = static_cast<int>(symbol::sym_list_splat_pattern),
  };
  enum class fields {
    fld_type = static_cast<int>(field::fld_type),
  };
};
struct sym_unary_operator {
  enum class fields {
    fld_argument = static_cast<int>(field::fld_argument),
    fld_operator = static_cast<int>(field::fld_operator),
  };
};
struct sym_while_statement {
  enum class fields {
    fld_alternative = static_cast<int>(field::fld_alternative),
    fld_body = static_cast<int>(field::fld_body),
    fld_condition = static_cast<int>(field::fld_condition),
  };
};
struct sym_wildcard_import {};
struct sym_with_clause {
  enum class children {
    sym_with_item = static_cast<int>(symbol::sym_with_item),
  };
};
struct sym_with_item {
  enum class fields {
    fld_value = static_cast<int>(field::fld_value),
  };
};
struct sym_with_statement {
  enum class children {
    sym_with_clause = static_cast<int>(symbol::sym_with_clause),
  };
  enum class fields {
    fld_body = static_cast<int>(field::fld_body),
  };
};
struct sym_yield {
  enum class children {
    sym_expression_list = static_cast<int>(symbol::sym_expression_list),
  };
};
struct sym_comment {};
struct sym_ellipsis {};
struct sym_escape_sequence {};
struct sym_false {};
struct sym_float {};
struct sym_identifier {};
struct sym_integer {};
struct sym_none {};
struct sym_true {};
struct sym_type_conversion {};
}  // namespace stanly

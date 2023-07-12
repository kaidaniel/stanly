#pragma once

// generated using "generate_symbol_tables.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly {

enum class aliased_import {
  identifier = 1,
  dotted_name = 156,
};
enum class argument_list {
  list_splat = 144,
  dictionary_splat = 145,
  expression = 169,
  keyword_argument = 189,
  parenthesized_expression = 200,
};
enum class as_pattern {
  expression = 169,
  as_pattern_target = 241,
};
enum class assert_statement {
  expression = 169,
};
enum class assignment {
  expression_list = 155,
  pattern = 160,
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  pattern_list = 180,
  yield = 182,
  type = 188,
};
enum class attribute {
  identifier = 1,
  primary_expression = 170,
};
enum class augmented_assignment {
  expression_list = 155,
  pattern = 160,
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  pattern_list = 180,
  yield = 182,
};
enum class await {
  expression = 169,
};
enum class binary_operator {
  primary_expression = 170,
};
enum class block {};
enum class boolean_operator {
  expression = 169,
};
enum class call {
  argument_list = 151,
  primary_expression = 170,
  generator_expression = 198,
};
enum class case_clause {
  block = 154,
  if_clause = 203,
  case_pattern = 242,
};
enum class case_pattern {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
enum class chevron {
  expression = 169,
};
enum class class_definition {
  identifier = 1,
  argument_list = 151,
  block = 154,
};
enum class comparison_operator {
  primary_expression = 170,
};
enum class concatenated_string {
  string = 206,
};
enum class conditional_expression {
  expression = 169,
};
enum class decorated_definition {
  function_definition = 141,
  class_definition = 149,
  decorator = 153,
};
enum class decorator {
  primary_expression = 170,
};
enum class default_parameter {
  identifier = 1,
  expression = 169,
};
enum class delete_statement {
  expression_list = 155,
  expression = 169,
};
enum class dictionary {
  dictionary_splat = 145,
  pair = 194,
};
enum class dictionary_comprehension {
  pair = 194,
  for_in_clause = 202,
  if_clause = 203,
};
enum class dictionary_splat {
  expression = 169,
};
enum class dictionary_splat_pattern {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
enum class dotted_name {
  identifier = 1,
};
enum class elif_clause {
  block = 154,
  expression = 169,
};
enum class else_clause {
  block = 154,
};
enum class except_clause {
  block = 154,
  expression = 169,
};
enum class exec_statement {
  expression = 169,
  string = 206,
};
enum class expression_list {
  expression = 169,
};
enum class expression_statement {
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
enum class finally_clause {
  block = 154,
};
enum class for_in_clause {
  pattern = 160,
  expression = 169,
  pattern_list = 180,
};
enum class for_statement {
  else_clause = 130,
  block = 154,
  expression_list = 155,
  pattern = 160,
  expression = 169,
  pattern_list = 180,
};
enum class format_expression {
  expression = 169,
};
enum class format_specifier {
  format_expression = 210,
};
enum class function_definition {
  identifier = 1,
  parameters = 142,
  block = 154,
  type = 188,
};
enum class future_import_statement {
  aliased_import = 114,
  dotted_name = 156,
};
enum class generator_expression {
  expression = 169,
  for_in_clause = 202,
  if_clause = 203,
};
enum class global_statement {
  identifier = 1,
};
enum class if_clause {
  expression = 169,
};
enum class if_statement {
  elif_clause = 129,
  else_clause = 130,
  block = 154,
  expression = 169,
};
enum class import_from_statement {
  relative_import = 110,
  aliased_import = 114,
  wildcard_import = 115,
  dotted_name = 156,
};
enum class import_statement {
  aliased_import = 114,
  dotted_name = 156,
};
enum class interpolation {
  type_conversion = 91,
  expression = 169,
  format_specifier = 209,
};
enum class keyword_argument {
  identifier = 1,
  expression = 169,
};
enum class lambda {
  lambda_parameters = 143,
  expression = 169,
};
enum class lambda_parameters {
  parameter = 159,
};
enum class list {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
enum class list_comprehension {
  expression = 169,
  for_in_clause = 202,
  if_clause = 203,
};
enum class list_pattern {
  pattern = 160,
};
enum class list_splat {
  expression = 169,
};
enum class list_splat_pattern {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
enum class match_statement {
  case_clause = 132,
  expression = 169,
};
enum class module {};
enum class named_expression {
  identifier = 1,
  expression = 169,
};
enum class nonlocal_statement {
  identifier = 1,
};
enum class not_operator {
  expression = 169,
};
enum class pair {
  expression = 169,
};
enum class parameters {
  parameter = 159,
};
enum class parenthesized_expression {
  list_splat = 144,
  expression = 169,
  yield = 182,
  parenthesized_expression = 200,
};
enum class parenthesized_list_splat {
  list_splat = 144,
  parenthesized_expression = 200,
};
enum class pattern_list {
  pattern = 160,
};
enum class print_statement {
  chevron = 117,
  expression = 169,
};
enum class raise_statement {
  expression_list = 155,
  expression = 169,
};
enum class relative_import {
  import_prefix = 109,
  dotted_name = 156,
};
enum class return_statement {
  expression_list = 155,
  expression = 169,
};
enum class set {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
enum class set_comprehension {
  expression = 169,
  for_in_clause = 202,
  if_clause = 203,
};
enum class slice {
  expression = 169,
};
enum class string {
  escape_sequence = 88,
  interpolation = 207,
};
enum class subscript {
  expression = 169,
  primary_expression = 170,
  slice = 185,
};
enum class try_statement {
  else_clause = 130,
  except_clause = 136,
  finally_clause = 137,
  block = 154,
};
enum class tuple {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
enum class tuple_pattern {
  pattern = 160,
};
enum class type {
  expression = 169,
};
enum class typed_default_parameter {
  identifier = 1,
  expression = 169,
  type = 188,
};
enum class typed_parameter {
  identifier = 1,
  list_splat_pattern = 165,
  dictionary_splat_pattern = 166,
  type = 188,
};
enum class unary_operator {
  primary_expression = 170,
};
enum class while_statement {
  else_clause = 130,
  block = 154,
  expression = 169,
};
enum class with_clause {
  with_item = 140,
};
enum class with_item {
  expression = 169,
};
enum class with_statement {
  with_clause = 139,
  block = 154,
};
enum class yield {
  expression_list = 155,
  expression = 169,
};
enum class _compound_statement {
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
enum class _simple_statement {
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
enum class expression {
  named_expression = 120,
  as_pattern = 167,
  primary_expression = 170,
  not_operator = 171,
  boolean_operator = 172,
  comparison_operator = 175,
  lambda = 176,
  conditional_expression = 204,
  await = 211,
};
enum class parameter {
  identifier = 1,
  tuple_pattern = 161,
  default_parameter = 163,
  typed_default_parameter = 164,
  list_splat_pattern = 165,
  dictionary_splat_pattern = 166,
  typed_parameter = 187,
  positional_separator = 212,
  keyword_separator = 213,
};
enum class pattern {
  identifier = 1,
  tuple_pattern = 161,
  list_pattern = 162,
  list_splat_pattern = 165,
  attribute = 183,
  subscript = 184,
};
enum class primary_expression {
  identifier = 1,
  ellipsis = 83,
  integer = 92,
  s_float = 93,
  s_true = 95,
  s_false = 96,
  none = 97,
  binary_operator = 173,
  unary_operator = 174,
  attribute = 183,
  subscript = 184,
  call = 186,
  list = 190,
  set = 191,
  tuple = 192,
  dictionary = 193,
  list_comprehension = 195,
  dictionary_comprehension = 196,
  set_comprehension = 197,
  generator_expression = 198,
  parenthesized_expression = 200,
  concatenated_string = 205,
  string = 206,
};
}  // namespace stanly

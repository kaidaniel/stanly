#pragma once

// generated using "generate_parser_symbols.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly::parser_symbols {

enum class symbols {
  identifier = 1,
  ellipsis = 83,
  escape_sequence = 88,
  type_conversion = 91,
  integer = 92,
  s_float = 93,
  s_true = 95,
  s_false = 96,
  none = 97,
  comment = 98,
  module = 105,
  import_statement = 108,
  import_prefix = 109,
  relative_import = 110,
  future_import_statement = 111,
  import_from_statement = 112,
  aliased_import = 114,
  wildcard_import = 115,
  print_statement = 116,
  chevron = 117,
  assert_statement = 118,
  expression_statement = 119,
  named_expression = 120,
  return_statement = 122,
  delete_statement = 123,
  raise_statement = 124,
  pass_statement = 125,
  break_statement = 126,
  continue_statement = 127,
  if_statement = 128,
  elif_clause = 129,
  else_clause = 130,
  match_statement = 131,
  case_clause = 132,
  for_statement = 133,
  while_statement = 134,
  try_statement = 135,
  except_clause = 136,
  finally_clause = 137,
  with_statement = 138,
  with_clause = 139,
  with_item = 140,
  function_definition = 141,
  parameters = 142,
  lambda_parameters = 143,
  list_splat = 144,
  dictionary_splat = 145,
  global_statement = 146,
  nonlocal_statement = 147,
  exec_statement = 148,
  class_definition = 149,
  parenthesized_list_splat = 150,
  argument_list = 151,
  decorated_definition = 152,
  decorator = 153,
  block = 154,
  expression_list = 155,
  dotted_name = 156,
  parameter = 159,
  pattern = 160,
  tuple_pattern = 161,
  list_pattern = 162,
  default_parameter = 163,
  typed_default_parameter = 164,
  list_splat_pattern = 165,
  dictionary_splat_pattern = 166,
  as_pattern = 167,
  expression = 169,
  primary_expression = 170,
  not_operator = 171,
  boolean_operator = 172,
  binary_operator = 173,
  unary_operator = 174,
  comparison_operator = 175,
  lambda = 176,
  assignment = 178,
  augmented_assignment = 179,
  pattern_list = 180,
  yield = 182,
  attribute = 183,
  subscript = 184,
  slice = 185,
  call = 186,
  typed_parameter = 187,
  type = 188,
  keyword_argument = 189,
  list = 190,
  set = 191,
  tuple = 192,
  dictionary = 193,
  pair = 194,
  list_comprehension = 195,
  dictionary_comprehension = 196,
  set_comprehension = 197,
  generator_expression = 198,
  parenthesized_expression = 200,
  for_in_clause = 202,
  if_clause = 203,
  conditional_expression = 204,
  concatenated_string = 205,
  string = 206,
  interpolation = 207,
  format_specifier = 209,
  format_expression = 210,
  await = 211,
  positional_separator = 212,
  keyword_separator = 213,
  case_pattern = 242,
};
enum class fields {
  alias = 1,
  alternative = 2,
  argument = 3,
  arguments = 4,
  attribute = 5,
  body = 6,
  cause = 7,
  code = 8,
  condition = 9,
  consequence = 10,
  definition = 11,
  function = 12,
  guard = 13,
  key = 14,
  left = 15,
  module_name = 16,
  name = 17,
  object = 18,
  s_operator = 19,
  operators = 20,
  parameters = 21,
  pattern = 22,
  return_type = 23,
  right = 24,
  subject = 25,
  subscript = 26,
  superclasses = 27,
  type = 28,
  value = 29,
};
namespace _compound_statement {
enum class subtypes {
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
}
namespace _simple_statement {
enum class subtypes {
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
}
namespace expression {
enum class subtypes {
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
}
namespace parameter {
enum class subtypes {
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
}
namespace pattern {
enum class subtypes {
  identifier = 1,
  tuple_pattern = 161,
  list_pattern = 162,
  list_splat_pattern = 165,
  attribute = 183,
  subscript = 184,
};
}
namespace primary_expression {
enum class subtypes {
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
}
namespace argument_list {
enum class children {
  list_splat = 144,
  dictionary_splat = 145,
  expression = 169,
  keyword_argument = 189,
  parenthesized_expression = 200,
};
}
namespace as_pattern {
enum class children {
  expression = 169,
};
}
namespace assert_statement {
enum class children {
  expression = 169,
};
}
namespace await {
enum class children {
  expression = 169,
};
}
namespace block {
}
namespace case_pattern {
enum class children {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
}
namespace chevron {
enum class children {
  expression = 169,
};
}
namespace comparison_operator {
enum class children {
  primary_expression = 170,
};
}
namespace concatenated_string {
enum class children {
  string = 206,
};
}
namespace conditional_expression {
enum class children {
  expression = 169,
};
}
namespace decorated_definition {
enum class children {
  decorator = 153,
};
}
namespace decorator {
enum class children {
  primary_expression = 170,
};
}
namespace delete_statement {
enum class children {
  expression_list = 155,
  expression = 169,
};
}
namespace dictionary {
enum class children {
  dictionary_splat = 145,
  pair = 194,
};
}
namespace dictionary_comprehension {
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
}
namespace dictionary_splat {
enum class children {
  expression = 169,
};
}
namespace dictionary_splat_pattern {
enum class children {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
}
namespace dotted_name {
enum class children {
  identifier = 1,
};
}
namespace except_clause {
enum class children {
  block = 154,
  expression = 169,
};
}
namespace exec_statement {
enum class children {
  expression = 169,
};
}
namespace expression_list {
enum class children {
  expression = 169,
};
}
namespace expression_statement {
enum class children {
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
}
namespace finally_clause {
enum class children {
  block = 154,
};
}
namespace format_expression {
enum class children {
  expression = 169,
};
}
namespace format_specifier {
enum class children {
  format_expression = 210,
};
}
namespace generator_expression {
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
}
namespace global_statement {
enum class children {
  identifier = 1,
};
}
namespace if_clause {
enum class children {
  expression = 169,
};
}
namespace import_from_statement {
enum class children {
  wildcard_import = 115,
};
}
namespace interpolation {
enum class children {
  type_conversion = 91,
  expression = 169,
  format_specifier = 209,
};
}
namespace lambda_parameters {
enum class children {
  parameter = 159,
};
}
namespace list {
enum class children {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
}
namespace list_comprehension {
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
}
namespace list_pattern {
enum class children {
  pattern = 160,
};
}
namespace list_splat {
enum class children {
  expression = 169,
};
}
namespace list_splat_pattern {
enum class children {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
}
namespace module {
}
namespace nonlocal_statement {
enum class children {
  identifier = 1,
};
}
namespace parameters {
enum class children {
  parameter = 159,
};
}
namespace parenthesized_expression {
enum class children {
  list_splat = 144,
  expression = 169,
  yield = 182,
  parenthesized_expression = 200,
};
}
namespace parenthesized_list_splat {
enum class children {
  list_splat = 144,
  parenthesized_expression = 200,
};
}
namespace pattern_list {
enum class children {
  pattern = 160,
};
}
namespace print_statement {
enum class children {
  chevron = 117,
};
}
namespace raise_statement {
enum class children {
  expression_list = 155,
  expression = 169,
};
}
namespace relative_import {
enum class children {
  import_prefix = 109,
  dotted_name = 156,
};
}
namespace return_statement {
enum class children {
  expression_list = 155,
  expression = 169,
};
}
namespace set {
enum class children {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
}
namespace set_comprehension {
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
}
namespace slice {
enum class children {
  expression = 169,
};
}
namespace string {
enum class children {
  escape_sequence = 88,
  interpolation = 207,
};
}
namespace try_statement {
enum class children {
  else_clause = 130,
  except_clause = 136,
  finally_clause = 137,
};
}
namespace tuple {
enum class children {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
}
namespace tuple_pattern {
enum class children {
  pattern = 160,
};
}
namespace type {
enum class children {
  expression = 169,
};
}
namespace typed_parameter {
enum class children {
  identifier = 1,
  list_splat_pattern = 165,
  dictionary_splat_pattern = 166,
};
}
namespace with_clause {
enum class children {
  with_item = 140,
};
}
namespace with_statement {
enum class children {
  with_clause = 139,
};
}
namespace yield {
enum class children {
  expression_list = 155,
  expression = 169,
};
}
namespace aliased_import {
enum class field_names {
  alias = 1,
  name = 17,
};
}
namespace aliased_import::fields {
enum class alias {
  identifier = 1,
};
}
namespace aliased_import::fields {
enum class name {
  dotted_name = 156,
};
}
namespace as_pattern {
enum class field_names {
  alias = 1,
};
}
namespace as_pattern::fields {
enum class alias {
  as_pattern_target = 241,
};
}
namespace assignment {
enum class field_names {
  left = 15,
  right = 24,
  type = 28,
};
}
namespace assignment::fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
}
namespace assignment::fields {
enum class right {
  expression_list = 155,
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
}
namespace assignment::fields {
enum class type {
  type = 188,
};
}
namespace attribute {
enum class field_names {
  attribute = 5,
  object = 18,
};
}
namespace attribute::fields {
enum class attribute {
  identifier = 1,
};
}
namespace attribute::fields {
enum class object {
  primary_expression = 170,
};
}
namespace augmented_assignment {
enum class field_names {
  left = 15,
  s_operator = 19,
  right = 24,
};
}
namespace augmented_assignment::fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
}
namespace augmented_assignment::fields {
}
namespace augmented_assignment::fields {
enum class right {
  expression_list = 155,
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
}
namespace binary_operator {
enum class field_names {
  left = 15,
  s_operator = 19,
  right = 24,
};
}
namespace binary_operator::fields {
enum class left {
  primary_expression = 170,
};
}
namespace binary_operator::fields {
}
namespace binary_operator::fields {
enum class right {
  primary_expression = 170,
};
}
namespace boolean_operator {
enum class field_names {
  left = 15,
  s_operator = 19,
  right = 24,
};
}
namespace boolean_operator::fields {
enum class left {
  expression = 169,
};
}
namespace boolean_operator::fields {
}
namespace boolean_operator::fields {
enum class right {
  expression = 169,
};
}
namespace call {
enum class field_names {
  arguments = 4,
  function = 12,
};
}
namespace call::fields {
enum class arguments {
  argument_list = 151,
  generator_expression = 198,
};
}
namespace call::fields {
enum class function {
  primary_expression = 170,
};
}
namespace case_clause {
enum class field_names {
  consequence = 10,
  guard = 13,
  pattern = 22,
};
}
namespace case_clause::fields {
enum class consequence {
  block = 154,
};
}
namespace case_clause::fields {
enum class guard {
  if_clause = 203,
};
}
namespace case_clause::fields {
enum class pattern {
  case_pattern = 242,
};
}
namespace class_definition {
enum class field_names {
  body = 6,
  name = 17,
  superclasses = 27,
};
}
namespace class_definition::fields {
enum class body {
  block = 154,
};
}
namespace class_definition::fields {
enum class name {
  identifier = 1,
};
}
namespace class_definition::fields {
enum class superclasses {
  argument_list = 151,
};
}
namespace comparison_operator {
enum class field_names {
  operators = 20,
};
}
namespace comparison_operator::fields {
}
namespace decorated_definition {
enum class field_names {
  definition = 11,
};
}
namespace decorated_definition::fields {
enum class definition {
  function_definition = 141,
  class_definition = 149,
};
}
namespace default_parameter {
enum class field_names {
  name = 17,
  value = 29,
};
}
namespace default_parameter::fields {
enum class name {
  identifier = 1,
};
}
namespace default_parameter::fields {
enum class value {
  expression = 169,
};
}
namespace dictionary_comprehension {
enum class field_names {
  body = 6,
};
}
namespace dictionary_comprehension::fields {
enum class body {
  pair = 194,
};
}
namespace elif_clause {
enum class field_names {
  condition = 9,
  consequence = 10,
};
}
namespace elif_clause::fields {
enum class condition {
  expression = 169,
};
}
namespace elif_clause::fields {
enum class consequence {
  block = 154,
};
}
namespace else_clause {
enum class field_names {
  body = 6,
};
}
namespace else_clause::fields {
enum class body {
  block = 154,
};
}
namespace exec_statement {
enum class field_names {
  code = 8,
};
}
namespace exec_statement::fields {
enum class code {
  string = 206,
};
}
namespace for_in_clause {
enum class field_names {
  left = 15,
  right = 24,
};
}
namespace for_in_clause::fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
}
namespace for_in_clause::fields {
enum class right {
  expression = 169,
};
}
namespace for_statement {
enum class field_names {
  alternative = 2,
  body = 6,
  left = 15,
  right = 24,
};
}
namespace for_statement::fields {
enum class alternative {
  else_clause = 130,
};
}
namespace for_statement::fields {
enum class body {
  block = 154,
};
}
namespace for_statement::fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
}
namespace for_statement::fields {
enum class right {
  expression_list = 155,
  expression = 169,
};
}
namespace function_definition {
enum class field_names {
  body = 6,
  name = 17,
  parameters = 21,
  return_type = 23,
};
}
namespace function_definition::fields {
enum class body {
  block = 154,
};
}
namespace function_definition::fields {
enum class name {
  identifier = 1,
};
}
namespace function_definition::fields {
enum class parameters {
  parameters = 142,
};
}
namespace function_definition::fields {
enum class return_type {
  type = 188,
};
}
namespace future_import_statement {
enum class field_names {
  name = 17,
};
}
namespace future_import_statement::fields {
enum class name {
  aliased_import = 114,
  dotted_name = 156,
};
}
namespace generator_expression {
enum class field_names {
  body = 6,
};
}
namespace generator_expression::fields {
enum class body {
  expression = 169,
};
}
namespace if_statement {
enum class field_names {
  alternative = 2,
  condition = 9,
  consequence = 10,
};
}
namespace if_statement::fields {
enum class alternative {
  elif_clause = 129,
  else_clause = 130,
};
}
namespace if_statement::fields {
enum class condition {
  expression = 169,
};
}
namespace if_statement::fields {
enum class consequence {
  block = 154,
};
}
namespace import_from_statement {
enum class field_names {
  module_name = 16,
  name = 17,
};
}
namespace import_from_statement::fields {
enum class module_name {
  relative_import = 110,
  dotted_name = 156,
};
}
namespace import_from_statement::fields {
enum class name {
  aliased_import = 114,
  dotted_name = 156,
};
}
namespace import_statement {
enum class field_names {
  name = 17,
};
}
namespace import_statement::fields {
enum class name {
  aliased_import = 114,
  dotted_name = 156,
};
}
namespace keyword_argument {
enum class field_names {
  name = 17,
  value = 29,
};
}
namespace keyword_argument::fields {
enum class name {
  identifier = 1,
};
}
namespace keyword_argument::fields {
enum class value {
  expression = 169,
};
}
namespace lambda {
enum class field_names {
  body = 6,
  parameters = 21,
};
}
namespace lambda::fields {
enum class body {
  expression = 169,
};
}
namespace lambda::fields {
enum class parameters {
  lambda_parameters = 143,
};
}
namespace list_comprehension {
enum class field_names {
  body = 6,
};
}
namespace list_comprehension::fields {
enum class body {
  expression = 169,
};
}
namespace match_statement {
enum class field_names {
  alternative = 2,
  subject = 25,
};
}
namespace match_statement::fields {
enum class alternative {
  case_clause = 132,
};
}
namespace match_statement::fields {
enum class subject {
  expression = 169,
};
}
namespace named_expression {
enum class field_names {
  name = 17,
  value = 29,
};
}
namespace named_expression::fields {
enum class name {
  identifier = 1,
};
}
namespace named_expression::fields {
enum class value {
  expression = 169,
};
}
namespace not_operator {
enum class field_names {
  argument = 3,
};
}
namespace not_operator::fields {
enum class argument {
  expression = 169,
};
}
namespace pair {
enum class field_names {
  key = 14,
  value = 29,
};
}
namespace pair::fields {
enum class key {
  expression = 169,
};
}
namespace pair::fields {
enum class value {
  expression = 169,
};
}
namespace print_statement {
enum class field_names {
  argument = 3,
};
}
namespace print_statement::fields {
enum class argument {
  expression = 169,
};
}
namespace raise_statement {
enum class field_names {
  cause = 7,
};
}
namespace raise_statement::fields {
enum class cause {
  expression = 169,
};
}
namespace set_comprehension {
enum class field_names {
  body = 6,
};
}
namespace set_comprehension::fields {
enum class body {
  expression = 169,
};
}
namespace subscript {
enum class field_names {
  subscript = 26,
  value = 29,
};
}
namespace subscript::fields {
enum class subscript {
  expression = 169,
  slice = 185,
};
}
namespace subscript::fields {
enum class value {
  primary_expression = 170,
};
}
namespace try_statement {
enum class field_names {
  body = 6,
};
}
namespace try_statement::fields {
enum class body {
  block = 154,
};
}
namespace typed_default_parameter {
enum class field_names {
  name = 17,
  type = 28,
  value = 29,
};
}
namespace typed_default_parameter::fields {
enum class name {
  identifier = 1,
};
}
namespace typed_default_parameter::fields {
enum class type {
  type = 188,
};
}
namespace typed_default_parameter::fields {
enum class value {
  expression = 169,
};
}
namespace typed_parameter {
enum class field_names {
  type = 28,
};
}
namespace typed_parameter::fields {
enum class type {
  type = 188,
};
}
namespace unary_operator {
enum class field_names {
  argument = 3,
  s_operator = 19,
};
}
namespace unary_operator::fields {
enum class argument {
  primary_expression = 170,
};
}
namespace unary_operator::fields {
}
namespace while_statement {
enum class field_names {
  alternative = 2,
  body = 6,
  condition = 9,
};
}
namespace while_statement::fields {
enum class alternative {
  else_clause = 130,
};
}
namespace while_statement::fields {
enum class body {
  block = 154,
};
}
namespace while_statement::fields {
enum class condition {
  expression = 169,
};
}
namespace with_item {
enum class field_names {
  value = 29,
};
}
namespace with_item::fields {
enum class value {
  expression = 169,
};
}
namespace with_statement {
enum class field_names {
  body = 6,
};
}
namespace with_statement::fields {
enum class body {
  block = 154,
};
}
}  // namespace stanly::parser_symbols

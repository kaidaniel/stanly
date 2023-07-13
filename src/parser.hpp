#pragma once

// generated using "generate_parser_symbols.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly::parser {

enum class terminals;
struct parser;
void
parse(parser&, enum terminals);

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

enum class terminals {
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
  import_prefix = 109,
  wildcard_import = 115,
  pass_statement = 125,
  break_statement = 126,
  continue_statement = 127,
  positional_separator = 212,
  keyword_separator = 213,
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
void
parse(parser&);
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
}  // namespace _compound_statement

namespace _simple_statement {
void
parse(parser&);
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
}  // namespace _simple_statement

namespace expression {
void
parse(parser&);
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
}  // namespace expression

namespace parameter {
void
parse(parser&);
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
}  // namespace parameter

namespace pattern {
void
parse(parser&);
enum class subtypes {
  identifier = 1,
  tuple_pattern = 161,
  list_pattern = 162,
  list_splat_pattern = 165,
  attribute = 183,
  subscript = 184,
};
}  // namespace pattern

namespace primary_expression {
void
parse(parser&);
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
}  // namespace primary_expression

namespace aliased_import {
void
parse(parser&);
enum class field_names {
  alias = 1,
  name = 17,
};
namespace fields {
enum class alias {
  identifier = 1,
};
enum class name {
  dotted_name = 156,
};
}  // namespace fields
}  // namespace aliased_import

namespace argument_list {
void
parse(parser&);
enum class children {
  list_splat = 144,
  dictionary_splat = 145,
  expression = 169,
  keyword_argument = 189,
  parenthesized_expression = 200,
};
}  // namespace argument_list

namespace as_pattern {
void
parse(parser&);
enum class children {
  expression = 169,
};
enum class field_names {
  alias = 1,
};
namespace fields {
enum class alias {
  as_pattern_target = 241,
};
}
}  // namespace as_pattern

namespace assert_statement {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace assert_statement

namespace assignment {
void
parse(parser&);
enum class field_names {
  left = 15,
  right = 24,
  type = 28,
};
namespace fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
enum class right {
  expression_list = 155,
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
enum class type {
  type = 188,
};
}  // namespace fields
}  // namespace assignment

namespace attribute {
void
parse(parser&);
enum class field_names {
  attribute = 5,
  object = 18,
};
namespace fields {
enum class attribute {
  identifier = 1,
};
enum class object {
  primary_expression = 170,
};
}  // namespace fields
}  // namespace attribute

namespace augmented_assignment {
void
parse(parser&);
enum class field_names {
  left = 15,
  s_operator = 19,
  right = 24,
};
namespace fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
enum class right {
  expression_list = 155,
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
}  // namespace fields
}  // namespace augmented_assignment

namespace await {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace await

namespace binary_operator {
void
parse(parser&);
enum class field_names {
  left = 15,
  s_operator = 19,
  right = 24,
};
namespace fields {
enum class left {
  primary_expression = 170,
};
enum class right {
  primary_expression = 170,
};
}  // namespace fields
}  // namespace binary_operator

namespace block {
void
parse(parser&);
}  // namespace block

namespace boolean_operator {
void
parse(parser&);
enum class field_names {
  left = 15,
  s_operator = 19,
  right = 24,
};
namespace fields {
enum class left {
  expression = 169,
};
enum class right {
  expression = 169,
};
}  // namespace fields
}  // namespace boolean_operator

namespace call {
void
parse(parser&);
enum class field_names {
  arguments = 4,
  function = 12,
};
namespace fields {
enum class arguments {
  argument_list = 151,
  generator_expression = 198,
};
enum class function {
  primary_expression = 170,
};
}  // namespace fields
}  // namespace call

namespace case_clause {
void
parse(parser&);
enum class field_names {
  consequence = 10,
  guard = 13,
  pattern = 22,
};
namespace fields {
enum class consequence {
  block = 154,
};
enum class guard {
  if_clause = 203,
};
enum class pattern {
  case_pattern = 242,
};
}  // namespace fields
}  // namespace case_clause

namespace case_pattern {
void
parse(parser&);
enum class children {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
}  // namespace case_pattern

namespace chevron {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace chevron

namespace class_definition {
void
parse(parser&);
enum class field_names {
  body = 6,
  name = 17,
  superclasses = 27,
};
namespace fields {
enum class body {
  block = 154,
};
enum class name {
  identifier = 1,
};
enum class superclasses {
  argument_list = 151,
};
}  // namespace fields
}  // namespace class_definition

namespace comparison_operator {
void
parse(parser&);
enum class children {
  primary_expression = 170,
};
enum class field_names {
  operators = 20,
};
namespace fields {
}
}  // namespace comparison_operator

namespace concatenated_string {
void
parse(parser&);
enum class children {
  string = 206,
};
}  // namespace concatenated_string

namespace conditional_expression {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace conditional_expression

namespace decorated_definition {
void
parse(parser&);
enum class children {
  decorator = 153,
};
enum class field_names {
  definition = 11,
};
namespace fields {
enum class definition {
  function_definition = 141,
  class_definition = 149,
};
}
}  // namespace decorated_definition

namespace decorator {
void
parse(parser&);
enum class children {
  primary_expression = 170,
};
}  // namespace decorator

namespace default_parameter {
void
parse(parser&);
enum class field_names {
  name = 17,
  value = 29,
};
namespace fields {
enum class name {
  identifier = 1,
};
enum class value {
  expression = 169,
};
}  // namespace fields
}  // namespace default_parameter

namespace delete_statement {
void
parse(parser&);
enum class children {
  expression_list = 155,
  expression = 169,
};
}  // namespace delete_statement

namespace dictionary {
void
parse(parser&);
enum class children {
  dictionary_splat = 145,
  pair = 194,
};
}  // namespace dictionary

namespace dictionary_comprehension {
void
parse(parser&);
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  pair = 194,
};
}
}  // namespace dictionary_comprehension

namespace dictionary_splat {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace dictionary_splat

namespace dictionary_splat_pattern {
void
parse(parser&);
enum class children {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
}  // namespace dictionary_splat_pattern

namespace dotted_name {
void
parse(parser&);
enum class children {
  identifier = 1,
};
}  // namespace dotted_name

namespace elif_clause {
void
parse(parser&);
enum class field_names {
  condition = 9,
  consequence = 10,
};
namespace fields {
enum class condition {
  expression = 169,
};
enum class consequence {
  block = 154,
};
}  // namespace fields
}  // namespace elif_clause

namespace else_clause {
void
parse(parser&);
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  block = 154,
};
}
}  // namespace else_clause

namespace except_clause {
void
parse(parser&);
enum class children {
  block = 154,
  expression = 169,
};
}  // namespace except_clause

namespace exec_statement {
void
parse(parser&);
enum class children {
  expression = 169,
};
enum class field_names {
  code = 8,
};
namespace fields {
enum class code {
  string = 206,
};
}
}  // namespace exec_statement

namespace expression_list {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace expression_list

namespace expression_statement {
void
parse(parser&);
enum class children {
  expression = 169,
  assignment = 178,
  augmented_assignment = 179,
  yield = 182,
};
}  // namespace expression_statement

namespace finally_clause {
void
parse(parser&);
enum class children {
  block = 154,
};
}  // namespace finally_clause

namespace for_in_clause {
void
parse(parser&);
enum class field_names {
  left = 15,
  right = 24,
};
namespace fields {
enum class left {
  pattern = 160,
  pattern_list = 180,
};
enum class right {
  expression = 169,
};
}  // namespace fields
}  // namespace for_in_clause

namespace for_statement {
void
parse(parser&);
enum class field_names {
  alternative = 2,
  body = 6,
  left = 15,
  right = 24,
};
namespace fields {
enum class alternative {
  else_clause = 130,
};
enum class body {
  block = 154,
};
enum class left {
  pattern = 160,
  pattern_list = 180,
};
enum class right {
  expression_list = 155,
  expression = 169,
};
}  // namespace fields
}  // namespace for_statement

namespace format_expression {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace format_expression

namespace format_specifier {
void
parse(parser&);
enum class children {
  format_expression = 210,
};
}  // namespace format_specifier

namespace function_definition {
void
parse(parser&);
enum class field_names {
  body = 6,
  name = 17,
  parameters = 21,
  return_type = 23,
};
namespace fields {
enum class body {
  block = 154,
};
enum class name {
  identifier = 1,
};
enum class parameters {
  parameters = 142,
};
enum class return_type {
  type = 188,
};
}  // namespace fields
}  // namespace function_definition

namespace future_import_statement {
void
parse(parser&);
enum class field_names {
  name = 17,
};
namespace fields {
enum class name {
  aliased_import = 114,
  dotted_name = 156,
};
}
}  // namespace future_import_statement

namespace generator_expression {
void
parse(parser&);
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  expression = 169,
};
}
}  // namespace generator_expression

namespace global_statement {
void
parse(parser&);
enum class children {
  identifier = 1,
};
}  // namespace global_statement

namespace if_clause {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace if_clause

namespace if_statement {
void
parse(parser&);
enum class field_names {
  alternative = 2,
  condition = 9,
  consequence = 10,
};
namespace fields {
enum class alternative {
  elif_clause = 129,
  else_clause = 130,
};
enum class condition {
  expression = 169,
};
enum class consequence {
  block = 154,
};
}  // namespace fields
}  // namespace if_statement

namespace import_from_statement {
void
parse(parser&);
enum class children {
  wildcard_import = 115,
};
enum class field_names {
  module_name = 16,
  name = 17,
};
namespace fields {
enum class module_name {
  relative_import = 110,
  dotted_name = 156,
};
enum class name {
  aliased_import = 114,
  dotted_name = 156,
};
}  // namespace fields
}  // namespace import_from_statement

namespace import_statement {
void
parse(parser&);
enum class field_names {
  name = 17,
};
namespace fields {
enum class name {
  aliased_import = 114,
  dotted_name = 156,
};
}
}  // namespace import_statement

namespace interpolation {
void
parse(parser&);
enum class children {
  type_conversion = 91,
  expression = 169,
  format_specifier = 209,
};
}  // namespace interpolation

namespace keyword_argument {
void
parse(parser&);
enum class field_names {
  name = 17,
  value = 29,
};
namespace fields {
enum class name {
  identifier = 1,
};
enum class value {
  expression = 169,
};
}  // namespace fields
}  // namespace keyword_argument

namespace lambda {
void
parse(parser&);
enum class field_names {
  body = 6,
  parameters = 21,
};
namespace fields {
enum class body {
  expression = 169,
};
enum class parameters {
  lambda_parameters = 143,
};
}  // namespace fields
}  // namespace lambda

namespace lambda_parameters {
void
parse(parser&);
enum class children {
  parameter = 159,
};
}  // namespace lambda_parameters

namespace list {
void
parse(parser&);
enum class children {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
}  // namespace list

namespace list_comprehension {
void
parse(parser&);
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  expression = 169,
};
}
}  // namespace list_comprehension

namespace list_pattern {
void
parse(parser&);
enum class children {
  pattern = 160,
};
}  // namespace list_pattern

namespace list_splat {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace list_splat

namespace list_splat_pattern {
void
parse(parser&);
enum class children {
  identifier = 1,
  attribute = 183,
  subscript = 184,
};
}  // namespace list_splat_pattern

namespace match_statement {
void
parse(parser&);
enum class field_names {
  alternative = 2,
  subject = 25,
};
namespace fields {
enum class alternative {
  case_clause = 132,
};
enum class subject {
  expression = 169,
};
}  // namespace fields
}  // namespace match_statement

namespace module {
void
parse(parser&);
}  // namespace module

namespace named_expression {
void
parse(parser&);
enum class field_names {
  name = 17,
  value = 29,
};
namespace fields {
enum class name {
  identifier = 1,
};
enum class value {
  expression = 169,
};
}  // namespace fields
}  // namespace named_expression

namespace nonlocal_statement {
void
parse(parser&);
enum class children {
  identifier = 1,
};
}  // namespace nonlocal_statement

namespace not_operator {
void
parse(parser&);
enum class field_names {
  argument = 3,
};
namespace fields {
enum class argument {
  expression = 169,
};
}
}  // namespace not_operator

namespace pair {
void
parse(parser&);
enum class field_names {
  key = 14,
  value = 29,
};
namespace fields {
enum class key {
  expression = 169,
};
enum class value {
  expression = 169,
};
}  // namespace fields
}  // namespace pair

namespace parameters {
void
parse(parser&);
enum class children {
  parameter = 159,
};
}  // namespace parameters

namespace parenthesized_expression {
void
parse(parser&);
enum class children {
  list_splat = 144,
  expression = 169,
  yield = 182,
  parenthesized_expression = 200,
};
}  // namespace parenthesized_expression

namespace parenthesized_list_splat {
void
parse(parser&);
enum class children {
  list_splat = 144,
  parenthesized_expression = 200,
};
}  // namespace parenthesized_list_splat

namespace pattern_list {
void
parse(parser&);
enum class children {
  pattern = 160,
};
}  // namespace pattern_list

namespace print_statement {
void
parse(parser&);
enum class children {
  chevron = 117,
};
enum class field_names {
  argument = 3,
};
namespace fields {
enum class argument {
  expression = 169,
};
}
}  // namespace print_statement

namespace raise_statement {
void
parse(parser&);
enum class children {
  expression_list = 155,
  expression = 169,
};
enum class field_names {
  cause = 7,
};
namespace fields {
enum class cause {
  expression = 169,
};
}
}  // namespace raise_statement

namespace relative_import {
void
parse(parser&);
enum class children {
  import_prefix = 109,
  dotted_name = 156,
};
}  // namespace relative_import

namespace return_statement {
void
parse(parser&);
enum class children {
  expression_list = 155,
  expression = 169,
};
}  // namespace return_statement

namespace set {
void
parse(parser&);
enum class children {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
}  // namespace set

namespace set_comprehension {
void
parse(parser&);
enum class children {
  for_in_clause = 202,
  if_clause = 203,
};
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  expression = 169,
};
}
}  // namespace set_comprehension

namespace slice {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace slice

namespace string {
void
parse(parser&);
enum class children {
  escape_sequence = 88,
  interpolation = 207,
};
}  // namespace string

namespace subscript {
void
parse(parser&);
enum class field_names {
  subscript = 26,
  value = 29,
};
namespace fields {
enum class subscript {
  expression = 169,
  slice = 185,
};
enum class value {
  primary_expression = 170,
};
}  // namespace fields
}  // namespace subscript

namespace try_statement {
void
parse(parser&);
enum class children {
  else_clause = 130,
  except_clause = 136,
  finally_clause = 137,
};
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  block = 154,
};
}
}  // namespace try_statement

namespace tuple {
void
parse(parser&);
enum class children {
  list_splat = 144,
  parenthesized_list_splat = 150,
  expression = 169,
  yield = 182,
};
}  // namespace tuple

namespace tuple_pattern {
void
parse(parser&);
enum class children {
  pattern = 160,
};
}  // namespace tuple_pattern

namespace type {
void
parse(parser&);
enum class children {
  expression = 169,
};
}  // namespace type

namespace typed_default_parameter {
void
parse(parser&);
enum class field_names {
  name = 17,
  type = 28,
  value = 29,
};
namespace fields {
enum class name {
  identifier = 1,
};
enum class type {
  type = 188,
};
enum class value {
  expression = 169,
};
}  // namespace fields
}  // namespace typed_default_parameter

namespace typed_parameter {
void
parse(parser&);
enum class children {
  identifier = 1,
  list_splat_pattern = 165,
  dictionary_splat_pattern = 166,
};
enum class field_names {
  type = 28,
};
namespace fields {
enum class type {
  type = 188,
};
}
}  // namespace typed_parameter

namespace unary_operator {
void
parse(parser&);
enum class field_names {
  argument = 3,
  s_operator = 19,
};
namespace fields {
enum class argument {
  primary_expression = 170,
};
}  // namespace fields
}  // namespace unary_operator

namespace while_statement {
void
parse(parser&);
enum class field_names {
  alternative = 2,
  body = 6,
  condition = 9,
};
namespace fields {
enum class alternative {
  else_clause = 130,
};
enum class body {
  block = 154,
};
enum class condition {
  expression = 169,
};
}  // namespace fields
}  // namespace while_statement

namespace with_clause {
void
parse(parser&);
enum class children {
  with_item = 140,
};
}  // namespace with_clause

namespace with_item {
void
parse(parser&);
enum class field_names {
  value = 29,
};
namespace fields {
enum class value {
  expression = 169,
};
}
}  // namespace with_item

namespace with_statement {
void
parse(parser&);
enum class children {
  with_clause = 139,
};
enum class field_names {
  body = 6,
};
namespace fields {
enum class body {
  block = 154,
};
}
}  // namespace with_statement

namespace yield {
void
parse(parser&);
enum class children {
  expression_list = 155,
  expression = 169,
};
}  // namespace yield
}  // namespace stanly::parser

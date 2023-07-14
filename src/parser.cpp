#include "parser.hpp"

// generated using "generate_parser_symbols.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly::parser {

void
f();

namespace sym__compound_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym__compound_statement::parse not  yet implemented";
}
}  // namespace sym__compound_statement
namespace sym__simple_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym__simple_statement::parse not  yet implemented";
}
}  // namespace sym__simple_statement
namespace sym_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_expression::parse not  yet implemented";
}
}  // namespace sym_expression
namespace sym_parameter {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_parameter::parse not  yet implemented";
}
}  // namespace sym_parameter
namespace sym_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_pattern::parse not  yet implemented";
}
}  // namespace sym_pattern
namespace sym_primary_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_primary_expression::parse not  yet implemented";
}
}  // namespace sym_primary_expression
namespace sym_aliased_import {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_aliased_import::parse not  yet implemented";
}
}  // namespace sym_aliased_import
namespace sym_argument_list {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_argument_list::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_dictionary_splat: break;
    case sym_expression: break;
    case sym_keyword_argument: break;
    case sym_list_splat: break;
    case sym_parenthesized_expression: break;
  }
}
}  // namespace sym_argument_list
namespace sym_as_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_as_pattern::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_as_pattern
namespace sym_assert_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_assert_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_assert_statement
namespace sym_assignment {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_assignment::parse not  yet implemented";
}
}  // namespace sym_assignment
namespace sym_attribute {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_attribute::parse not  yet implemented";
}
}  // namespace sym_attribute
namespace sym_augmented_assignment {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_augmented_assignment::parse not  yet implemented";
}
}  // namespace sym_augmented_assignment
namespace sym_await {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_await::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_await
namespace sym_binary_operator {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_binary_operator::parse not  yet implemented";
}
}  // namespace sym_binary_operator
namespace sym_block {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_block::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym__compound_statement: break;
    case sym__simple_statement: break;
  }
}
}  // namespace sym_block
namespace sym_boolean_operator {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_boolean_operator::parse not  yet implemented";
}
}  // namespace sym_boolean_operator
namespace sym_call {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_call::parse not  yet implemented";
}
}  // namespace sym_call
namespace sym_case_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_case_clause::parse not  yet implemented";
}
}  // namespace sym_case_clause
namespace sym_case_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_case_pattern::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_attribute: break;
    case sym_identifier: break;
    case sym_subscript: break;
  }
}
}  // namespace sym_case_pattern
namespace sym_chevron {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_chevron::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_chevron
namespace sym_class_definition {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_class_definition::parse not  yet implemented";
}
}  // namespace sym_class_definition
namespace sym_comparison_operator {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_comparison_operator::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_primary_expression: break;
  }
}
}  // namespace sym_comparison_operator
namespace sym_concatenated_string {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_concatenated_string::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_string: break;
  }
}
}  // namespace sym_concatenated_string
namespace sym_conditional_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_conditional_expression::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_conditional_expression
namespace sym_decorated_definition {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_decorated_definition::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_decorator: break;
  }
}
}  // namespace sym_decorated_definition
namespace sym_decorator {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_decorator::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_primary_expression: break;
  }
}
}  // namespace sym_decorator
namespace sym_default_parameter {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_default_parameter::parse not  yet implemented";
}
}  // namespace sym_default_parameter
namespace sym_delete_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_delete_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_expression_list: break;
  }
}
}  // namespace sym_delete_statement
namespace sym_dictionary {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_dictionary::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_dictionary_splat: break;
    case sym_pair: break;
  }
}
}  // namespace sym_dictionary
namespace sym_dictionary_comprehension {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_dictionary_comprehension::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_for_in_clause: break;
    case sym_if_clause: break;
  }
}
}  // namespace sym_dictionary_comprehension
namespace sym_dictionary_splat {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_dictionary_splat::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_dictionary_splat
namespace sym_dictionary_splat_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_dictionary_splat_pattern::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_attribute: break;
    case sym_identifier: break;
    case sym_subscript: break;
  }
}
}  // namespace sym_dictionary_splat_pattern
namespace sym_dotted_name {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_dotted_name::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_identifier: break;
  }
}
}  // namespace sym_dotted_name
namespace sym_elif_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_elif_clause::parse not  yet implemented";
}
}  // namespace sym_elif_clause
namespace sym_else_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_else_clause::parse not  yet implemented";
}
}  // namespace sym_else_clause
namespace sym_except_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_except_clause::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_block: break;
    case sym_expression: break;
  }
}
}  // namespace sym_except_clause
namespace sym_exec_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_exec_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_exec_statement
namespace sym_expression_list {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_expression_list::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_expression_list
namespace sym_expression_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_expression_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_assignment: break;
    case sym_augmented_assignment: break;
    case sym_expression: break;
    case sym_yield: break;
  }
}
}  // namespace sym_expression_statement
namespace sym_finally_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_finally_clause::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_block: break;
  }
}
}  // namespace sym_finally_clause
namespace sym_for_in_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_for_in_clause::parse not  yet implemented";
}
}  // namespace sym_for_in_clause
namespace sym_for_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_for_statement::parse not  yet implemented";
}
}  // namespace sym_for_statement
namespace sym_format_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_format_expression::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_format_expression
namespace sym_format_specifier {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_format_specifier::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_format_expression: break;
  }
}
}  // namespace sym_format_specifier
namespace sym_function_definition {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_function_definition::parse not  yet implemented";
}
}  // namespace sym_function_definition
namespace sym_future_import_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_future_import_statement::parse not  yet implemented";
}
}  // namespace sym_future_import_statement
namespace sym_generator_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_generator_expression::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_for_in_clause: break;
    case sym_if_clause: break;
  }
}
}  // namespace sym_generator_expression
namespace sym_global_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_global_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_identifier: break;
  }
}
}  // namespace sym_global_statement
namespace sym_if_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_if_clause::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_if_clause
namespace sym_if_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_if_statement::parse not  yet implemented";
}
}  // namespace sym_if_statement
namespace sym_import_from_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_import_from_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_wildcard_import: break;
  }
}
}  // namespace sym_import_from_statement
namespace sym_import_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_import_statement::parse not  yet implemented";
}
}  // namespace sym_import_statement
namespace sym_interpolation {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_interpolation::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_format_specifier: break;
    case sym_type_conversion: break;
  }
}
}  // namespace sym_interpolation
namespace sym_keyword_argument {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_keyword_argument::parse not  yet implemented";
}
}  // namespace sym_keyword_argument
namespace sym_lambda {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_lambda::parse not  yet implemented";
}
}  // namespace sym_lambda
namespace sym_lambda_parameters {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_lambda_parameters::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_parameter: break;
  }
}
}  // namespace sym_lambda_parameters
namespace sym_list {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_list::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_list_splat: break;
    case sym_parenthesized_list_splat: break;
    case sym_yield: break;
  }
}
}  // namespace sym_list
namespace sym_list_comprehension {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_list_comprehension::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_for_in_clause: break;
    case sym_if_clause: break;
  }
}
}  // namespace sym_list_comprehension
namespace sym_list_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_list_pattern::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_pattern: break;
  }
}
}  // namespace sym_list_pattern
namespace sym_list_splat {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_list_splat::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_list_splat
namespace sym_list_splat_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_list_splat_pattern::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_attribute: break;
    case sym_identifier: break;
    case sym_subscript: break;
  }
}
}  // namespace sym_list_splat_pattern
namespace sym_match_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_match_statement::parse not  yet implemented";
}
}  // namespace sym_match_statement
namespace sym_module {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_module::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym__compound_statement: break;
    case sym__simple_statement: break;
  }
}
}  // namespace sym_module
namespace sym_named_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_named_expression::parse not  yet implemented";
}
}  // namespace sym_named_expression
namespace sym_nonlocal_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_nonlocal_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_identifier: break;
  }
}
}  // namespace sym_nonlocal_statement
namespace sym_not_operator {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_not_operator::parse not  yet implemented";
}
}  // namespace sym_not_operator
namespace sym_pair {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_pair::parse not  yet implemented";
}
}  // namespace sym_pair
namespace sym_parameters {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_parameters::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_parameter: break;
  }
}
}  // namespace sym_parameters
namespace sym_parenthesized_expression {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_parenthesized_expression::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_list_splat: break;
    case sym_parenthesized_expression: break;
    case sym_yield: break;
  }
}
}  // namespace sym_parenthesized_expression
namespace sym_parenthesized_list_splat {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_parenthesized_list_splat::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_list_splat: break;
    case sym_parenthesized_expression: break;
  }
}
}  // namespace sym_parenthesized_list_splat
namespace sym_pattern_list {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_pattern_list::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_pattern: break;
  }
}
}  // namespace sym_pattern_list
namespace sym_print_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_print_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_chevron: break;
  }
}
}  // namespace sym_print_statement
namespace sym_raise_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_raise_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_expression_list: break;
  }
}
}  // namespace sym_raise_statement
namespace sym_relative_import {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_relative_import::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_dotted_name: break;
    case sym_import_prefix: break;
  }
}
}  // namespace sym_relative_import
namespace sym_return_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_return_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_expression_list: break;
  }
}
}  // namespace sym_return_statement
namespace sym_set {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_set::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_list_splat: break;
    case sym_parenthesized_list_splat: break;
    case sym_yield: break;
  }
}
}  // namespace sym_set
namespace sym_set_comprehension {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_set_comprehension::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_for_in_clause: break;
    case sym_if_clause: break;
  }
}
}  // namespace sym_set_comprehension
namespace sym_slice {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_slice::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_slice
namespace sym_string {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_string::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_escape_sequence: break;
    case sym_interpolation: break;
  }
}
}  // namespace sym_string
namespace sym_subscript {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_subscript::parse not  yet implemented";
}
}  // namespace sym_subscript
namespace sym_try_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_try_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_else_clause: break;
    case sym_except_clause: break;
    case sym_finally_clause: break;
  }
}
}  // namespace sym_try_statement
namespace sym_tuple {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_tuple::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_list_splat: break;
    case sym_parenthesized_list_splat: break;
    case sym_yield: break;
  }
}
}  // namespace sym_tuple
namespace sym_tuple_pattern {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_tuple_pattern::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_pattern: break;
  }
}
}  // namespace sym_tuple_pattern
namespace sym_type {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_type::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
  }
}
}  // namespace sym_type
namespace sym_typed_default_parameter {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_typed_default_parameter::parse not  yet implemented";
}
}  // namespace sym_typed_default_parameter
namespace sym_typed_parameter {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_typed_parameter::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_dictionary_splat_pattern: break;
    case sym_identifier: break;
    case sym_list_splat_pattern: break;
  }
}
}  // namespace sym_typed_parameter
namespace sym_unary_operator {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_unary_operator::parse not  yet implemented";
}
}  // namespace sym_unary_operator
namespace sym_while_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_while_statement::parse not  yet implemented";
}
}  // namespace sym_while_statement
namespace sym_with_clause {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_with_clause::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_with_item: break;
  }
}
}  // namespace sym_with_clause
namespace sym_with_item {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_with_item::parse not  yet implemented";
}
}  // namespace sym_with_item
namespace sym_with_statement {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_with_statement::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_with_clause: break;
  }
}
}  // namespace sym_with_statement
namespace sym_yield {
void
parse(parser&, enum symbols s) {
  throw "stanly::parser::sym_yield::parse not  yet implemented";
  switch (static_cast<children>(s)) {
    using enum children;
    case sym_expression: break;
    case sym_expression_list: break;
  }
}
}  // namespace sym_yield
}  // namespace stanly::parser

#include <string_view>
#include <utility>

#include "parser.hpp"

// generated using "generate_parser_symbols.sh"
// nodes_json="build-default/tree-sitter-python/src/node-types.json"
// lookup_symbols="build-default/src/lookup-symbols"

namespace stanly::parser {

void f();

void
parse_sym__compound_statement(parser& p) {
  auto children = parse_children(p);
  /*
    subtypes:
      class_definition
      decorated_definition
      for_statement
      function_definition
      if_statement
      match_statement
      try_statement
      while_statement
      with_statement
  */
}
void
parse_sym__simple_statement(parser& p) {
  auto children = parse_children(p);
  /*
    subtypes:
      assert_statement
      break_statement
      continue_statement
      delete_statement
      exec_statement
      expression_statement
      future_import_statement
      global_statement
      import_from_statement
      import_statement
      nonlocal_statement
      pass_statement
      print_statement
      raise_statement
      return_statement
  */
}
void
parse_sym_expression(parser& p) {
  auto children = parse_children(p);
  /*
    subtypes:
      as_pattern
      await
      boolean_operator
      comparison_operator
      conditional_expression
      lambda
      named_expression
      not_operator
      primary_expression
  */
}
void
parse_sym_parameter(parser& p) {
  auto children = parse_children(p);
  /*
    subtypes:
      default_parameter
      dictionary_splat_pattern
      identifier
      keyword_separator
      list_splat_pattern
      positional_separator
      tuple_pattern
      typed_default_parameter
      typed_parameter
  */
}
void
parse_sym_pattern(parser& p) {
  auto children = parse_children(p);
  /*
    subtypes:
      attribute
      identifier
      list_pattern
      list_splat_pattern
      subscript
      tuple_pattern
  */
}
void
parse_sym_primary_expression(parser& p) {
  auto children = parse_children(p);
  /*
    subtypes:
      attribute
      binary_operator
      call
      concatenated_string
      dictionary
      dictionary_comprehension
      ellipsis
      false
      float
      generator_expression
      identifier
      integer
      list
      list_comprehension
      none
      parenthesized_expression
      set
      set_comprehension
      string
      subscript
      true
      tuple
      unary_operator
  */
}
void
parse_sym_aliased_import(parser& p) {
  auto sym_alias = parse_field(p, fields::sym_alias);
  auto sym_name = parse_field(p, fields::sym_name);
  auto children = parse_children(p);
  /*
    fields:
      alias:
        identifier
      name:
        dotted_name
  */
}
void
parse_sym_argument_list(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        dictionary_splat
        expression
        keyword_argument
        list_splat
        parenthesized_expression
  */
}
void
parse_sym_as_pattern(parser& p) {
  auto sym_alias = parse_field(p, fields::sym_alias);
  auto children = parse_children(p);
  /*
    fields:
      alias:
        as_pattern_target
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_assert_statement(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        expression
  */
}
void
parse_sym_assignment(parser& p) {
  auto sym_left = parse_field(p, fields::sym_left);
  auto sym_right = parse_field(p, fields::sym_right);
  auto sym_type = parse_field(p, fields::sym_type);
  auto children = parse_children(p);
  /*
    fields:
      left:
        pattern
        pattern_list
      right:
        assignment
        augmented_assignment
        expression
        expression_list
        yield
      type:
        type
  */
}
void
parse_sym_attribute(parser& p) {
  auto sym_attribute = parse_field(p, fields::sym_attribute);
  auto sym_object = parse_field(p, fields::sym_object);
  auto children = parse_children(p);
  /*
    fields:
      attribute:
        identifier
      object:
        primary_expression
  */
}
void
parse_sym_augmented_assignment(parser& p) {
  auto sym_left = parse_field(p, fields::sym_left);
  auto sym_operator = parse_field(p, fields::sym_operator);
  auto sym_right = parse_field(p, fields::sym_right);
  auto children = parse_children(p);
  /*
    fields:
      left:
        pattern
        pattern_list
      operator:
        %=
        &=
        **=
        *=
        +=
        -=
        //=
        /=
        <<=
        >>=
        @=
        ^=
        |=
      right:
        assignment
        augmented_assignment
        expression
        expression_list
        yield
  */
}
void
parse_sym_await(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_binary_operator(parser& p) {
  auto sym_left = parse_field(p, fields::sym_left);
  auto sym_operator = parse_field(p, fields::sym_operator);
  auto sym_right = parse_field(p, fields::sym_right);
  auto children = parse_children(p);
  /*
    fields:
      left:
        primary_expression
      operator:
        %
        &
        *
        **
        +
        -
        /
        //
        <<
        >>
        @
        ^
        |
      right:
        primary_expression
  */
}
void
parse_sym_block(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        _compound_statement
        _simple_statement
  */
}
void
parse_sym_boolean_operator(parser& p) {
  auto sym_left = parse_field(p, fields::sym_left);
  auto sym_operator = parse_field(p, fields::sym_operator);
  auto sym_right = parse_field(p, fields::sym_right);
  auto children = parse_children(p);
  /*
    fields:
      left:
        expression
      operator:
        and
        or
      right:
        expression
  */
}
void
parse_sym_break_statement(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_call(parser& p) {
  auto sym_arguments = parse_field(p, fields::sym_arguments);
  auto sym_function = parse_field(p, fields::sym_function);
  auto children = parse_children(p);
  /*
    fields:
      arguments:
        argument_list
        generator_expression
      function:
        primary_expression
  */
}
void
parse_sym_case_clause(parser& p) {
  auto sym_consequence = parse_field(p, fields::sym_consequence);
  auto sym_guard = parse_field(p, fields::sym_guard);
  auto sym_pattern = parse_field(p, fields::sym_pattern);
  auto children = parse_children(p);
  /*
    fields:
      consequence:
        block
      guard:
        if_clause
      pattern:
        case_pattern
  */
}
void
parse_sym_case_pattern(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        attribute
        identifier
        subscript
  */
}
void
parse_sym_chevron(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_class_definition(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto sym_name = parse_field(p, fields::sym_name);
  auto sym_superclasses = parse_field(p, fields::sym_superclasses);
  auto children = parse_children(p);
  /*
    fields:
      body:
        block
      name:
        identifier
      superclasses:
        argument_list
  */
}
void
parse_sym_comparison_operator(parser& p) {
  auto sym_operators = parse_field(p, fields::sym_operators);
  auto children = parse_children(p);
  /*
    fields:
      operators:
        !=
        <
        <=
        <>
        ==
        >
        >=
        in
        is
        not
    children:
      multiple: true
      required: true
      types:
        primary_expression
  */
}
void
parse_sym_concatenated_string(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        string
  */
}
void
parse_sym_conditional_expression(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        expression
  */
}
void
parse_sym_continue_statement(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_decorated_definition(parser& p) {
  auto sym_definition = parse_field(p, fields::sym_definition);
  auto children = parse_children(p);
  /*
    fields:
      definition:
        class_definition
        function_definition
    children:
      multiple: true
      required: true
      types:
        decorator
  */
}
void
parse_sym_decorator(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        primary_expression
  */
}
void
parse_sym_default_parameter(parser& p) {
  auto sym_name = parse_field(p, fields::sym_name);
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      name:
        identifier
      value:
        expression
  */
}
void
parse_sym_delete_statement(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
        expression_list
  */
}
void
parse_sym_dictionary(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        dictionary_splat
        pair
  */
}
void
parse_sym_dictionary_comprehension(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        pair
    children:
      multiple: true
      required: true
      types:
        for_in_clause
        if_clause
  */
}
void
parse_sym_dictionary_splat(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_dictionary_splat_pattern(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        attribute
        identifier
        subscript
  */
}
void
parse_sym_dotted_name(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        identifier
  */
}
void
parse_sym_elif_clause(parser& p) {
  auto sym_condition = parse_field(p, fields::sym_condition);
  auto sym_consequence = parse_field(p, fields::sym_consequence);
  auto children = parse_children(p);
  /*
    fields:
      condition:
        expression
      consequence:
        block
  */
}
void
parse_sym_else_clause(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        block
  */
}
void
parse_sym_except_clause(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        block
        expression
  */
}
void
parse_sym_exec_statement(parser& p) {
  auto sym_code = parse_field(p, fields::sym_code);
  auto children = parse_children(p);
  /*
    fields:
      code:
        string
    children:
      multiple: true
      required: false
      types:
        expression
  */
}
void
parse_sym_expression_list(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        expression
  */
}
void
parse_sym_expression_statement(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        assignment
        augmented_assignment
        expression
        yield
  */
}
void
parse_sym_finally_clause(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        block
  */
}
void
parse_sym_for_in_clause(parser& p) {
  auto sym_left = parse_field(p, fields::sym_left);
  auto sym_right = parse_field(p, fields::sym_right);
  auto children = parse_children(p);
  /*
    fields:
      left:
        pattern
        pattern_list
      right:
        expression
  */
}
void
parse_sym_for_statement(parser& p) {
  auto sym_alternative = parse_field(p, fields::sym_alternative);
  auto sym_body = parse_field(p, fields::sym_body);
  auto sym_left = parse_field(p, fields::sym_left);
  auto sym_right = parse_field(p, fields::sym_right);
  auto children = parse_children(p);
  /*
    fields:
      alternative:
        else_clause
      body:
        block
      left:
        pattern
        pattern_list
      right:
        expression
        expression_list
  */
}
void
parse_sym_format_expression(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_format_specifier(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        format_expression
  */
}
void
parse_sym_function_definition(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto sym_name = parse_field(p, fields::sym_name);
  auto sym_parameters = parse_field(p, fields::sym_parameters);
  auto sym_return_type = parse_field(p, fields::sym_return_type);
  auto children = parse_children(p);
  /*
    fields:
      body:
        block
      name:
        identifier
      parameters:
        parameters
      return_type:
        type
  */
}
void
parse_sym_future_import_statement(parser& p) {
  auto sym_name = parse_field(p, fields::sym_name);
  auto children = parse_children(p);
  /*
    fields:
      name:
        aliased_import
        dotted_name
  */
}
void
parse_sym_generator_expression(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        expression
    children:
      multiple: true
      required: true
      types:
        for_in_clause
        if_clause
  */
}
void
parse_sym_global_statement(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        identifier
  */
}
void
parse_sym_if_clause(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_if_statement(parser& p) {
  auto sym_alternative = parse_field(p, fields::sym_alternative);
  auto sym_condition = parse_field(p, fields::sym_condition);
  auto sym_consequence = parse_field(p, fields::sym_consequence);
  auto children = parse_children(p);
  /*
    fields:
      alternative:
        elif_clause
        else_clause
      condition:
        expression
      consequence:
        block
  */
}
void
parse_sym_import_from_statement(parser& p) {
  auto sym_module_name = parse_field(p, fields::sym_module_name);
  auto sym_name = parse_field(p, fields::sym_name);
  auto children = parse_children(p);
  /*
    fields:
      module_name:
        dotted_name
        relative_import
      name:
        aliased_import
        dotted_name
    children:
      multiple: false
      required: false
      types:
        wildcard_import
  */
}
void
parse_sym_import_prefix(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_import_statement(parser& p) {
  auto sym_name = parse_field(p, fields::sym_name);
  auto children = parse_children(p);
  /*
    fields:
      name:
        aliased_import
        dotted_name
  */
}
void
parse_sym_interpolation(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        expression
        format_specifier
        type_conversion
  */
}
void
parse_sym_keyword_argument(parser& p) {
  auto sym_name = parse_field(p, fields::sym_name);
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      name:
        identifier
      value:
        expression
  */
}
void
parse_sym_keyword_separator(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_lambda(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto sym_parameters = parse_field(p, fields::sym_parameters);
  auto children = parse_children(p);
  /*
    fields:
      body:
        expression
      parameters:
        lambda_parameters
  */
}
void
parse_sym_lambda_parameters(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        parameter
  */
}
void
parse_sym_list(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        expression
        list_splat
        parenthesized_list_splat
        yield
  */
}
void
parse_sym_list_comprehension(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        expression
    children:
      multiple: true
      required: true
      types:
        for_in_clause
        if_clause
  */
}
void
parse_sym_list_pattern(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        pattern
  */
}
void
parse_sym_list_splat(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_list_splat_pattern(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        attribute
        identifier
        subscript
  */
}
void
parse_sym_match_statement(parser& p) {
  auto sym_alternative = parse_field(p, fields::sym_alternative);
  auto sym_subject = parse_field(p, fields::sym_subject);
  auto children = parse_children(p);
  /*
    fields:
      alternative:
        case_clause
      subject:
        expression
  */
}
void
parse_sym_module(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        _compound_statement
        _simple_statement
  */
}
void
parse_sym_named_expression(parser& p) {
  auto sym_name = parse_field(p, fields::sym_name);
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      name:
        identifier
      value:
        expression
  */
}
void
parse_sym_nonlocal_statement(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        identifier
  */
}
void
parse_sym_not_operator(parser& p) {
  auto sym_argument = parse_field(p, fields::sym_argument);
  auto children = parse_children(p);
  /*
    fields:
      argument:
        expression
  */
}
void
parse_sym_pair(parser& p) {
  auto sym_key = parse_field(p, fields::sym_key);
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      key:
        expression
      value:
        expression
  */
}
void
parse_sym_parameters(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        parameter
  */
}
void
parse_sym_parenthesized_expression(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
        list_splat
        parenthesized_expression
        yield
  */
}
void
parse_sym_parenthesized_list_splat(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        list_splat
        parenthesized_expression
  */
}
void
parse_sym_pass_statement(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_pattern_list(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        pattern
  */
}
void
parse_sym_positional_separator(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_print_statement(parser& p) {
  auto sym_argument = parse_field(p, fields::sym_argument);
  auto children = parse_children(p);
  /*
    fields:
      argument:
        expression
    children:
      multiple: false
      required: false
      types:
        chevron
  */
}
void
parse_sym_raise_statement(parser& p) {
  auto sym_cause = parse_field(p, fields::sym_cause);
  auto children = parse_children(p);
  /*
    fields:
      cause:
        expression
    children:
      multiple: false
      required: false
      types:
        expression
        expression_list
  */
}
void
parse_sym_relative_import(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        dotted_name
        import_prefix
  */
}
void
parse_sym_return_statement(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: false
      types:
        expression
        expression_list
  */
}
void
parse_sym_set(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        expression
        list_splat
        parenthesized_list_splat
        yield
  */
}
void
parse_sym_set_comprehension(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        expression
    children:
      multiple: true
      required: true
      types:
        for_in_clause
        if_clause
  */
}
void
parse_sym_slice(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        expression
  */
}
void
parse_sym_string(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        escape_sequence
        interpolation
  */
}
void
parse_sym_subscript(parser& p) {
  auto sym_subscript = parse_field(p, fields::sym_subscript);
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      subscript:
        expression
        slice
      value:
        primary_expression
  */
}
void
parse_sym_try_statement(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        block
    children:
      multiple: true
      required: true
      types:
        else_clause
        except_clause
        finally_clause
  */
}
void
parse_sym_tuple(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        expression
        list_splat
        parenthesized_list_splat
        yield
  */
}
void
parse_sym_tuple_pattern(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: false
      types:
        pattern
  */
}
void
parse_sym_type(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: true
      types:
        expression
  */
}
void
parse_sym_typed_default_parameter(parser& p) {
  auto sym_name = parse_field(p, fields::sym_name);
  auto sym_type = parse_field(p, fields::sym_type);
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      name:
        identifier
      type:
        type
      value:
        expression
  */
}
void
parse_sym_typed_parameter(parser& p) {
  auto sym_type = parse_field(p, fields::sym_type);
  auto children = parse_children(p);
  /*
    fields:
      type:
        type
    children:
      multiple: false
      required: true
      types:
        dictionary_splat_pattern
        identifier
        list_splat_pattern
  */
}
void
parse_sym_unary_operator(parser& p) {
  auto sym_argument = parse_field(p, fields::sym_argument);
  auto sym_operator = parse_field(p, fields::sym_operator);
  auto children = parse_children(p);
  /*
    fields:
      argument:
        primary_expression
      operator:
        +
        -
        ~
  */
}
void
parse_sym_while_statement(parser& p) {
  auto sym_alternative = parse_field(p, fields::sym_alternative);
  auto sym_body = parse_field(p, fields::sym_body);
  auto sym_condition = parse_field(p, fields::sym_condition);
  auto children = parse_children(p);
  /*
    fields:
      alternative:
        else_clause
      body:
        block
      condition:
        expression
  */
}
void
parse_sym_wildcard_import(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_with_clause(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: true
      required: true
      types:
        with_item
  */
}
void
parse_sym_with_item(parser& p) {
  auto sym_value = parse_field(p, fields::sym_value);
  auto children = parse_children(p);
  /*
    fields:
      value:
        expression
  */
}
void
parse_sym_with_statement(parser& p) {
  auto sym_body = parse_field(p, fields::sym_body);
  auto children = parse_children(p);
  /*
    fields:
      body:
        block
    children:
      multiple: false
      required: true
      types:
        with_clause
  */
}
void
parse_sym_yield(parser& p) {
  auto children = parse_children(p);
  /*
    children:
      multiple: false
      required: false
      types:
        expression
        expression_list
  */
}
void
parse_sym_comment(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_ellipsis(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_escape_sequence(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_false(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_float(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_identifier(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_integer(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_none(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_true(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
void
parse_sym_type_conversion(parser& p) {
  auto children = parse_children(p);
  /*

  */
}
}  // namespace stanly::parser

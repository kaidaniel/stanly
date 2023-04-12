#include <catch2/catch_test_macros.hpp>

#include "firstorder-format.h"
#include "firstorder-syntax.h"
#include "parser.h"

namespace stanly::firstorder {
using std::bad_variant_access;
using std::format;
using std::get;
using std::index_sequence;
using std::make_index_sequence;
using std::size_t;
using std::string_view;
using std::tuple;

using stx = syntax<string_view>;
using load_var = stx::load_var;
using load_text = stx::load_text;
using load_top = stx::load_top;
using load_record = stx::load_record;
using load_field = stx::load_field;
using set_field = stx::set_field;

template <size_t... Idxs>
bool parses_to_impl(string_view program, const auto& nodes, index_sequence<Idxs...>) {
  auto parsed_vector{parse<stx>(program)};
  if (sizeof...(Idxs) != parsed_vector.size()) {
    constexpr string_view msg{"expected {} nodes, but parsed {}: {}"};
    UNSCOPED_INFO(std::format(msg, sizeof...(Idxs), parsed_vector.size(), parsed_vector));
    return false;
  }
  auto is_type = [&]<class N>(const N& node, const size_t i) -> bool {
    try {
      N parsed_node{get<N>(parsed_vector.at(i))};
      bool const result{to_tpl(parsed_node) == to_tpl(node)};
      if (!result) { UNSCOPED_INFO(format("expected {}, but got {}", node, parsed_node)); }
      return result;
    } catch (bad_variant_access&) {
      constexpr string_view msg{"statement #{} of \"{}\" parsed to wrong node '{}'"};
      UNSCOPED_INFO(format(msg, i + 1, program, parsed_vector.at(i)));
      return false;
    }
  };
  return (is_type(get<Idxs>(nodes), Idxs) && ...);
}

bool parses_to(string_view program, const auto&... node) {
  constexpr auto idxs = make_index_sequence<sizeof...(node)>{};
  return parses_to_impl(program, tuple<decltype(node)...>{node...}, idxs);
}

TEST_CASE("single statements", "[firstorder][parsing]") {
  CHECK(parses_to("x=y", load_var{"x", "y"}));
  CHECK(parses_to("x=1", load_text{"x", "1"}));
  CHECK(parses_to("y=[]", load_top{"y", "[]"}));
  CHECK(parses_to("z = {}", load_record{"z", {}}));
  CHECK(parses_to("z = {1: 'x', 3: {}}", load_record{"z", {"1", "3"}}));
  CHECK(parses_to("abc = {1: 'x'}", load_record{"abc", {"1"}}));
  CHECK(parses_to("abc_def = {1,2,3}", load_top{"abc_def", "{1,2,3}"}));
  CHECK(parses_to("a[b] = x", set_field{"x", "a", "b"}));
  CHECK(parses_to("x = a[b]", load_field{"x", "a", "b"}));
  CHECK(parses_to("x = y", load_var{"x", "y"}));
  CHECK(parses_to("x = 1", load_text{"x", "1"}));
}

TEST_CASE("multiple statements", "[firstorder][parsing]") {
  CHECK(parses_to("z={}; x=a[b]", load_record{"z", {}}, load_field{"x", "a", "b"}));
  CHECK(parses_to("x=y; y=[]", load_var{"x", "y"}, load_top{"y", "[]"}));
  CHECK(parses_to("x=y\ny=[]\nz=1", load_var{"x", "y"}, load_top{"y", "[]"}, load_text{"z", "1"}));
}

}  // namespace stanly::firstorder

// #include <format>

// template<typename T>
// struct std::formatter<stanly::firstorder::syntax<std::string_view>::node>

/*
// TODO remove the "." from ".first-order" to no longer skip the test.
TEST_CASE("add two elements to a record", "[.first-order][analysis]") {
  const auto graph = parse(
      R"python(
e = 1
f = 2
r = {}
r[e] = f
r[f] = e
)python");
  REQUIRE(format("{}", graph) == R"python(
(AssignLiteral e 1)
(AssignLiteral f 2)
(Local r)
(StoreSubscript f r e)
(StoreSubscript e r f)
)python");
  REQUIRE(format("{}", analyse(graph)) == R"python(
(Bind e (Integer 1))
(Bind f (Integer 2))
(Bind r (Record 1 2))
)python");
}
}
*/

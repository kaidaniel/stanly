#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <vector>

#include "firstorder-format.h"
#include "firstorder-syntax.h"
#include "parser.h"

namespace stanly::firstorder {
using fo = syntax<std::string_view>;

template <std::size_t... Idxs>
bool parses_to_impl(std::string_view program, const auto& nodes, std::index_sequence<Idxs...>) {
  std::vector parsed_vector{parse_language<fo>(program)};
  if (sizeof...(Idxs) != parsed_vector.size()) {
    constexpr std::string_view msg{"expected {} nodes, but parsed {}: {}"};
    UNSCOPED_INFO(std::format(msg, sizeof...(Idxs), parsed_vector.size(), parsed_vector));
    return false;
  }
  auto is_type = [&]<class N>(const N& node, const std::size_t i) -> bool {
    try {
      N parsed_node{std::get<N>(parsed_vector.at(i))};
      bool result{to_tpl(parsed_node) == to_tpl(node)};
      if (!result) { UNSCOPED_INFO(std::format("expected {}, but got {}", node, parsed_node)); }
      return result;
    } catch (std::bad_variant_access&) {
      constexpr std::string_view msg{"statement {} of \"{}\" parsed to wrong node '{}'"};
      UNSCOPED_INFO(std::format(msg, i + 1, program, parsed_vector.at(i)));
      return false;
    }
  };
  return (is_type(std::get<Idxs>(nodes), Idxs) && ...);
}

bool parses_to(std::string_view program, const auto&... node) {
  auto idxs = std::make_index_sequence<sizeof...(node)>{};
  return parses_to_impl(program, std::tuple<decltype(node)...>{node...}, idxs);
}
TEST_CASE("single statements", "[firstorder][parsing]") {
  CHECK(parses_to("x=y", fo::load_var{"x", "y"}));
  CHECK(parses_to("x=1", fo::load_text{"x", "1"}));
  CHECK(parses_to("y=[]", fo::load_top{"y", "[]"}));
  CHECK(parses_to("z = {}", fo::load_record{"z", {}}));
  CHECK(parses_to("z = {1: 'x', 3: {}}", fo::load_record{"z", {"1", "3"}}));
  CHECK(parses_to("abc = {1: 'x'}", fo::load_record{"abc", {"1"}}));
  CHECK(parses_to("abc_def = {1,2,3}", fo::load_top{"abc_def", "{1,2,3}"}));
  CHECK(parses_to("a[b] = x", fo::set_field{"x", "a", "b"}));
  CHECK(parses_to("x = a[b]", fo::load_field{"x", "a", "b"}));
  CHECK(parses_to("x = y", fo::load_var{"x", "y"}));
  CHECK(parses_to("x = 1", fo::load_text{"x", "1"}));
}

TEST_CASE("multiple statements", "[firstorder][parsing]") {
  CHECK(parses_to("z={}; x=a[b]", fo::load_record{"z", {}}, fo::load_field{"x", "a", "b"}));
  CHECK(parses_to("x=y; y=[]", fo::load_var{"x", "y"}, fo::load_top{"y", "[]"}));
  CHECK(parses_to("x=y\ny=[]\nz=1", fo::load_var{"x", "y"}, fo::load_top{"y", "[]"},
                  fo::load_text{"z", "1"}));
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

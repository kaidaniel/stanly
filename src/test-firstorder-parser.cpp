#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <vector>

#include "firstorder-format.h"
#include "firstorder-syntax.h"
#include "parser.h"

using std::string;
namespace stanly::firstorder {
using stx = syntax<std::string_view>;
constexpr auto parse = parse_language<syntax<std::string_view>>;

template <class Tpl, std::size_t... Idxs>
bool parses_to_(std::string_view program, const Tpl& nodes, std::index_sequence<Idxs...>) {
  std::vector parsed_vector{parse(program)};
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

template <class... Arg>
bool parses_to__(std::string_view program, const std::tuple<Arg...>& nodes) {
  auto idxs = std::make_index_sequence<std::tuple_size_v<std::decay_t<decltype(nodes)>>>{};
  return parses_to_(program, nodes, idxs);
}

template <class... Node>
bool parses_to(std::string_view program, const Node&... node) {
  return parses_to__(program, std::tuple<Node...>{node...});
}
TEST_CASE("single statements", "[firstorder][parsing]") {
  CHECK(parses_to("x=y", stx::load_var{"x", "y"}));
  CHECK(parses_to("x=1", stx::load_text{"x", "1"}));
  CHECK(parses_to("y=[]", stx::load_top{"y", "[]"}));
  CHECK(parses_to("z = {}", stx::load_record{"z", {}}));
  CHECK(parses_to("z = {1: 'x', 3: {}}", stx::load_record{"z", {"1", "3"}}));
  CHECK(parses_to("abc = {1: 'x'}", stx::load_record{"abc", {"1"}}));
  CHECK(parses_to("abc_def = {1,2,3}", stx::load_top{"abc_def", "{1,2,3}"}));
  CHECK(parses_to("a[b] = x", stx::set_field{"x", "a", "b"}));
  CHECK(parses_to("x = a[b]", stx::load_field{"x", "a", "b"}));
  CHECK(parses_to("x = y", stx::load_var{"x", "y"}));
  CHECK(parses_to("x = 1", stx::load_text{"x", "1"}));
}

TEST_CASE("multiple statements", "[firstorder][parsing]") {
  CHECK(parses_to("z={}; x=a[b]", stx::load_record{"z", {}}, stx::load_field{"x", "a", "b"}));
  CHECK(parses_to("x=y; y=[]", stx::load_var{"x", "y"}, stx::load_top{"y", "[]"}));
  CHECK(parses_to("x=y\ny=[]\nz=1", stx::load_var{"x", "y"}, stx::load_top{"y", "[]"},
                  stx::load_text{"z", "1"}));
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

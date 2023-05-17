#include <catch2/catch_test_macros.hpp>

#include "firstorder-syntax.h"
#include "parse.h"
#include "stanly-format.h"

namespace stanly::firstorder {
using std::decay_t;
using std::format;
using std::get_if;
using std::index_sequence;
using std::make_index_sequence;
using std::size_t;
using std::string_view;
using std::tuple;

using syn = syntax<string_view>;
using ref = syn::ref;
using text = syn::text;
using top = syn::top;
using record = syn::record;
using load = syn::load;
using store = syn::store;

template <class... Ts>
constexpr static size_t idx = -1;
template <class T, class... Tail>
constexpr static size_t idx<T, T, Tail...> = 0;
template <class T, class Head, class... Tail>
constexpr static size_t idx<T, Head, Tail...> = idx<T, Tail...> + 1;

template <class... T>
bool parses_to(string_view program, T... nodes) {
  auto parsed_vector{parse<syn::node>(program)};
  constexpr size_t n_nodes{sizeof...(T)};
  if (n_nodes != parsed_vector.size()) {
    constexpr string_view msg{"expected {} nodes, but parsed {}: {}"};
    UNSCOPED_INFO(format(msg, n_nodes, parsed_vector.size(), parsed_vector));
    return false;
  }
  auto vector_index_equals = [&]<class N>(const size_t idx, const N& node_i) -> bool {
    if (auto* parsed_node = get_if<N>(&parsed_vector.at(idx))) {
      bool const result = {to_tpl(*parsed_node) == to_tpl(node_i)};
      if (!result) { UNSCOPED_INFO(format("expected {}, but got {}", node_i, *parsed_node)); }
      return result;
    }
    constexpr string_view msg{"statement #{} of \"{}\" parsed to wrong node '{}'"};
    UNSCOPED_INFO(format(msg, idx + 1, program, parsed_vector.at(idx)));
    return false;
  };
  return (vector_index_equals(idx<T, T...>, get<T>(tuple{nodes...})) && ...);
}

TEST_CASE("parse firstorder single statements", "[firstorder][parser]") {
  CHECK(parses_to("x=y", ref{"x", "y"}));
  CHECK(parses_to("x=1", text{"x", "1"}));
  CHECK(parses_to("y=[]", top{"y", "[]"}));
  CHECK(parses_to("z = {}", record{"z", {}}));
  CHECK(parses_to("z = {1: 'x', 3: {}}", record{"z", {"1", "3"}}));
  CHECK(parses_to("abc = {1: 'x'}", record{"abc", {"1"}}));
  CHECK(parses_to("abc_def = {1,2,3}", top{"abc_def", "{1,2,3}"}));
  CHECK(parses_to("a[b] = x", store{"a", "b", "x"}));
  CHECK(parses_to("x = a[b]", load{"x", "a", "b"}));
  CHECK(parses_to("x = y", ref{"x", "y"}));
  CHECK(parses_to("x = 1", text{"x", "1"}));
}

TEST_CASE("parse firstorder multiple statements", "[firstorder][parser]") {
  CHECK(parses_to("z={}; x=a[b]", record{"z", {}}, load{"x", "a", "b"}));
  CHECK(parses_to("x=y; y=[]", ref{"x", "y"}, top{"y", "[]"}));
  CHECK(parses_to("x=y\ny=[]\nz=1", ref{"x", "y"}, top{"y", "[]"}, text{"z", "1"}));
  CHECK(parses_to("e=1; r={}; r[e]=f", text{"e", "1"}, record{"r", {}}, store{"r", "e", "f"}));
}
}  // namespace stanly::firstorder

/*
TEST_CASE("add two elements to a record", "[first-order][analysis]") {
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

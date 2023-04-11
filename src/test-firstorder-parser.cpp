
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <vector>

#include "firstorder-format.h"
#include "firstorder-syntax.h"
#include "parser.h"

using std::string;
namespace stanly::firstorder {
constexpr auto parse = parse_language<syntax<std::string_view>>;

TEST_CASE("single statements", "[first-order][parsing]") {
  auto v = GENERATE(chunk(2, values({
                                 // clang-format off
    "x=y",                  "[inj-load_var(x y)]",
    "x=1",                  "[inj-load_text(x 1)]",
    "y=[]",                 "[inj-load_top(y [])]",
    "z = {}",               "[inj-load_record(z [])]",
    "z = {1: 'x', 3: {}}",  "[inj-load_record(z [1, 3])]",
    "abc = {1: 'x'}",       "[inj-load_record(abc [1])]",
    "abc_def = {1,2,3}",    "[inj-load_top(abc_def {1,2,3})]",
    "a[b] = x",             "[inj-set_field(x a b)]",
    "x = a[b]",             "[inj-load_field(x a b)]",
    "x = y",                "[inj-load_var(x y)]",
    "x = 1",                "[inj-load_text(x 1)]",
                                 // clang-format on
                             })));
  auto statement = parse(v[0]);
  REQUIRE(std::format("{}", statement) == v[1]);
}
TEST_CASE("multiple statements", "[first-order][parsing]") {
  auto v =
      GENERATE(chunk(2, values({
                            // clang-format off
    "x=a[b]; z={}; y={2}",  "[inj-load_field(x a b), inj-load_record(z []), inj-load_top(y {2})]",
    "x=y; y=[]",            "[inj-load_var(x y), inj-load_top(y [])]",
    "x=y\ny=[]",            "[inj-load_var(x y), inj-load_top(y [])]",
                            // clang-format on
                        })));
  auto statement = parse(v[0]);
  REQUIRE(std::format("{}", statement) == v[1]);
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

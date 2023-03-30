#include <fmt/std.h>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <vector>

#include "firstorder-format.h"
#include "firstorder-syntax.h"
#include "parser.h"

using fmt::format;
using std::string;
namespace stanly::firstorder {
constexpr auto parse = parse_language<syntax<std::string_view>>;

TEST_CASE("parse single statements", "[first-order][parsing]") {
  // clang-format off
    auto v = GENERATE(chunk(2, values({
        "x=y",                  "ast[load_var(x y)]",
        "x=1",                  "ast[load_text(x 1)]",
        "y=[]",                 "ast[load_top(y [])]",
        "z = {}",               "ast[load_record(z [])]",
        "z = {1: 'x', 3: {}}",  "ast[load_record(z [1, 3])]",
        "abc = {1: 'x'}",       "ast[load_record(abc [1])]",
      // "abc_def = {1,2,3}",    "ast[load_top(abc_def [])]",
      //  "a[b] = x",             "ast[set_field(x a b)",
      //  "x = a[b]",             "ast[load_field(x a b)",
        "x = y",                "ast[load_var(x y)]",
        "x = 1",                "ast[load_text(x 1)]" 
    })));
  // clang-format on
  const string &statement = v[0];
  const string &translation = v[1];
  REQUIRE(format("{}", parse(statement)) == translation);
}
}  // namespace stanly::firstorder
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

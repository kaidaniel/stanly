#include <fmt/ranges.h>
#include <fmt/std.h>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <vector>

#include "firstorder-syntax.h"

using fmt::format;
using fmt::join;
using fmt::print;

namespace stanly::firstorder {
TEST_CASE("parse single statements", "[first-order][parsing]") {
  // clang-format off
    auto v = GENERATE(chunk(2, values({
        "x=y",                  "(LoadVar x=y)",
        "x=1",                  "(LoadText x='1')",
        "y=[]",                 "(DeclareLocalVar y)",
        "z = {}",               "(LoadRecord z=[])",
        "z = {1: 'x', 3: {}}",  "(LoadRecord z=[\"1\", \"3\"])",
    /*  "abc = {1: 'x'}",       "(LoadRecord abc (Record 1))",
        "abc_def = {1,2,3}",    "(Local abc_def)",
        "a[b] = x",             "(StoreSubscript x a b)",
        "x = a[b]",             "(LoadSubscript x a b)",
        "x = y",                "(AssignVar x y)",
        "x = 1",                "(AssignLiteral x 1)" */
    })));
  // clang-format on
  const std::string &statement = v[0];
  const std::string &translation = v[1];
  std::string res{};
  for (auto s : parse<syntax<std::string_view>>(statement)) {
    res.append(format("{}", s));
  }

  REQUIRE(res == translation);
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

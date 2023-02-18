#include "analysis.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <string>


TEST_CASE(
    "correctly parse programs consisting of a single statement",
    "[first-order][syntax]") {
  // clang-format off
    auto v = GENERATE(chunk(2, values({
        "x=input()",            "(Input x)",
        "y=[]",                 "(Local y)",
        "z = {}",               "(AllocRecord z (Record))",
        "z = {1: 'x', 3: {}}",  "(AllocRecord z (Record 1 3))",
        "abc = {1: 'x'}",       "(AllocRecord abc (Record 1))",
        "abc_def = {1,2,3}",    "(Local abc_def)",
        "a[b] = x",             "(StoreSubscript x a b)",
        "x = a[b]",             "(LoadSubscript x a b)",
        "x = y",                "(AssignVar x y)",
        "x = 1",                "(AssignLiteral x 1)"
    })));
  // clang-format on
  const std::string& statement = v[0];
  const std::string& translation = v[1];
  REQUIRE(ksar::parse(statement).show() == translation);
}

TEST_CASE("add two elements to a record", "[first-order][analysis]") {
  auto graph = ksar::parse(
      R"python(
e = 1
f = 2
r = {}
r[e] = f
r[f] = e
)python");
  REQUIRE(graph.show() == R"python(
(AssignLiteral e 1)
(AssignLiteral f 2)
(Local r)
(StoreSubscript f r e)
(StoreSubscript e r f)
)python");
  auto analysis = ksar::analyse(graph);
  REQUIRE(analysis.show() == R"python(
(Bind e (Integer 1))
(Bind f (Integer 2))
(Bind r (Record 1 2))
)python");
}

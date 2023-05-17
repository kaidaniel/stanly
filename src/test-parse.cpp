#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <ranges>

#include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "firstorder-syntax.h"
#include "parse.h"

namespace stanly::firstorder {
TEST_CASE("parse firstorder", "[parser]") {
  struct statements : syntax<std::string_view> {
    std::vector<std::pair<std::string, std::vector<node>>> value = {
        {"x=y", {ref{"x", "y"}}},
        {"x=1", {text{"x", "1"}}},
        {"y=[]", {top{"y", "[]"}}},
        {"z = {}", {record{"z", {}}}},
        {"z = {1: 'x', 3: {}}", {record{"z", {"1", "3"}}}},
        {"abc = {1: 'x'}", {record{"abc", {"1"}}}},
        {"abc_def = {1,2,3}", {top{"abc_def", "{1,2,3}"}}},
        {"a[b] = x", {store{"a", "b", "x"}}},
        {"x = a[b]", {load{"x", "a", "b"}}},
        {"x = y", {ref{"x", "y"}}},
        {"x = 1", {text{"x", "1"}}},
        {"z={}; x=a[b]", {record{"z"}, load{"x", "a", "b"}}},
        {"x=y; y=[]", {ref{"x", "y"}, top{"y", "[]"}}},
        {"x=y\ny=[]\nz=1", {ref{"x", "y"}, top{"y", "[]"}, text{"z", "1"}}},
        {"e=1;f=2;r={};r[e]=f",
         {text{"e", "1"}, text{"f", "2"}, record{"r"}, store{"r", "e", "f"}}},
    };
    static std::vector<node> pparse(std::string_view program) { return parse<node>(program); }
  };

  auto [program, nodes] = GENERATE(from_range(statements{}.value));
  CHECK_THAT(statements::pparse(program), Catch::Matchers::RangeEquals(nodes));
}
}  // namespace stanly::firstorder

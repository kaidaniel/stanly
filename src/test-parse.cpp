#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <ranges>

#include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "firstorder-syntax.h"
#include "parse.h"

namespace stanly::firstorder {

TEST_CASE("parse firstorder", "[parser]") {
  struct statements : syntax<std::string_view> {
    std::vector<std::pair<std::string_view, std::vector<node>>> operator()() {
      return {
          {"x=y", {ref{"x", "y"}}},
          {"x=1", {text{"x", "1"}}},
          {"y=[]", {alloc{"y", "top"}}},
          {"z = {}", {alloc{"z", "dict"}}},
          {"z = {1: 'x', 3: {}}",
           {alloc{"z", "dict"}, update{"z", "1", "'x'"}, update{"z", "3", "{}"}}},
          {"a = {1: 'x'}", {alloc{"a", "dict"}, update{"a", "1", "'x'"}}},
          {"a = {1,2,3}", {alloc{"a", "top"}}},
          {"a[b] = x", {update{"a", "b", "x"}}},
          {"x = a[b]", {load{"x", "a", "b"}}},
          {"x = y", {ref{"x", "y"}}},
          {"x = 1", {text{"x", "1"}}},
          {"z={}; x=a[b]", {alloc{"z", "dict"}, load{"x", "a", "b"}}},
          {"x=y; y=[]", {ref{"x", "y"}, alloc{"y", "top"}}},
          {"x=y\ny=[]\nz=1", {ref{"x", "y"}, alloc{"y", "top"}, text{"z", "1"}}},
          {"e=1;f=2;r={};r[e]=f",
           {text{"e", "1"}, text{"f", "2"}, alloc{"r", "dict"}, update{"r", "e", "f"}}},
      };
    }
  };

  auto [program, nodes] = GENERATE(from_range(statements{}()));
  CHECK(parse<syntax<std::string_view>::node>(program) == nodes);
  // CHECK_THAT(parse<syntax<std::string_view>::node>(program),
  // Catch::Matchers::RangeEquals(nodes));
}
}  // namespace stanly::firstorder

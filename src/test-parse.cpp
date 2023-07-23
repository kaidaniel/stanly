#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <string_view>
#include <vector>

#include "parse.h"
#include "string-index.h"
#include "syntax.h"

namespace stanly {

TEST_CASE("parse basic blocks of python programs", "[parser]") {
  auto [program, nodes] =
      GENERATE(from_range(std::vector<std::pair<std::string_view, std::vector<node>>>{
          {"x=y", {ref{"x"_h, "y"_h}}},
          {"x=1", {lit{"x"_h, "int"_h, "1"_h}}},
          {"x=f's'", {lit{"x"_h, "str"_h, "f's'"_h}}},
          {"y=[]", {alloc{"y"_h, "top"_h}}},
          {"z = {}", {alloc{"z"_h, "dict"_h}}},
          {"z = {1: 'x', 3: {}}",
           {alloc{"z"_h, "dict"_h}, update{"z"_h, "1"_h, "'x'"_h}, update{"z"_h, "3"_h, "{}"_h}}},
          {"a = {1: 'x'}", {alloc{"a"_h, "dict"_h}, update{"a"_h, "1"_h, "'x'"_h}}},
          {"a = {1,2,3}", {alloc{"a"_h, "top"_h}}},
          {"a[b] = x", {update{"a"_h, "b"_h, "x"_h}}},
          {"x = a[b]", {load{"x"_h, "a"_h, "b"_h}}},
          {"x = y", {ref{"x"_h, "y"_h}}},
          {"x = 1", {lit{"x"_h, "int"_h, "1"_h}}},
          {"z={}; x=a[b]", {alloc{"z"_h, "dict"_h}, load{"x"_h, "a"_h, "b"_h}}},
          {"x=y; y=[]", {ref{"x"_h, "y"_h}, alloc{"y"_h, "top"_h}}},
          {"x=y\ny=[]\nz=1",
           {ref{"x"_h, "y"_h}, alloc{"y"_h, "top"_h}, lit{"z"_h, "int"_h, "1"_h}}},
          {"e=1;f=2;r={};r[e]=f",
           {lit{"e"_h, "int"_h, "1"_h}, lit{"f"_h, "int"_h, "2"_h}, alloc{"r"_h, "dict"_h},
            update{"r"_h, "e"_h, "f"_h}}}}));
  CHECK(parse(std::string{program}) == nodes);
}
}  // namespace stanly

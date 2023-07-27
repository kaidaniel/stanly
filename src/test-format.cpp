#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <format>
#include <iostream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "catch2/generators/catch_generators.hpp"
#include "handle.h"
#include "stanly-utils.h"
#include "string-index.h"
#include "syntax.h"

namespace stanly {

TEST_CASE("format syntax nodes", "[format]") {
  SECTION("node") {
    auto [n, str] = GENERATE(from_range(std::vector<std::pair<node, std::string>>{
        {load{"a"_h, "b"_h, "c"_h}, std::format("{}(a b c)", type_name<load>)},
        {update{"a"_h, "b"_h, "c"_h}, std::format("{}(a b c)", type_name<update>)},
        {lit{"a"_h, "integer"_h, "b"_h}, std::format("{}(a integer b)", type_name<lit>)},
        {alloc{"a"_h, "dict"_h}, std::format("{}(a dict)", type_name<alloc>)},
        {ref{"a"_h, "b"_h}, std::format("{}(a b)", type_name<ref>)}}));
    CHECK(resolve_handles(n) == str);
  }
  SECTION("handle node") {
    auto [n, str] = GENERATE(from_range(std::vector<std::pair<node, std::string>>{
        {{load{0_i, 1_i, 2_i}, std::format("{}(0 1 2)", type_name<load>)},
         {update{3_i, 4_i, 5_i}, std::format("{}(3 4 5)", type_name<update>)},
         {lit{6_i, 1_i, 7_i}, std::format("{}(6 1 7)", type_name<lit>)},
         {alloc{8_i, 9_i}, std::format("{}(8 9)", type_name<alloc>)},
         {ref{10_i, 11_i}, std::format("{}(10 11)", type_name<ref>)}}}));
    CHECK(std::format("{}", n) == "inj-" + str);
  }
  SECTION("std::vector<node>") {
    auto [n, str] = GENERATE(from_range(std::vector<std::pair<std::vector<node>, std::string>>{
        {{load{"a"_h, "b"_h, "c"_h}, lit{"a"_h, "s"_h, "b"_h}},
         std::format("[{}(a b c), {}(a s b)]", type_name<load>, type_name<lit>)},
        {{ref{"a"_h, "b"_h}, alloc{"a"_h, "top"_h}},
         std::format("[{}(a b), {}(a top)]", type_name<ref>, type_name<alloc>)},
        {{}, "[]"}}));
    CHECK(std::format("{}", resolve_handles(n)) == str);
  }
  SECTION("std::unordered_map<std::string_view, node>") {
    auto [map, str] = GENERATE(
        from_range(std::vector<std::pair<std::unordered_map<std::string_view, node>, std::string>>{
            {{{"2", alloc{"c"_h, "top"_h}},
              {"0", lit{"a"_h, "s"_h, "b"_h}},
              {"3", update{"a"_h, "b"_h, "c"_h}}},
             std::format(
                 "{}0: {}(a s b), 2: {}(c top), 3: {}(a b c){}", "{", type_name<lit>,
                 type_name<alloc>, type_name<update>, "}")},
            {{}, "{}"}}));
    CHECK(std::format("{}", resolve_handles(map)) == str);
  }
  SECTION("std::vector, std::unordered_map") {
    std::unordered_map<handle, node> map{{0_i, alloc{1_i, 2_i}}, {3_i, lit{3_i, 1_i, 4_i}}};
    CHECK(
        std::format("{}", map) ==
        std::format(
            "{}3: inj-{}(3 1 4), 0: inj-{}(1 2){}", "{", type_name<lit>, type_name<alloc>, "}"));
    CHECK(
        std::format("{}", std::vector<node>{update{5_i, 6_i, 7_i}, lit{8_i, 1_i, 9_i}}) ==
        std::format("[inj-{}(5 6 7), inj-{}(8 1 9)]", type_name<update>, type_name<lit>));
  }
}

namespace detail {
static_assert(requires(node node) { std::cout << node; });
static_assert(requires(update update) { std::cout << update; });
}  // namespace detail

}  // namespace stanly

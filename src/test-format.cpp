#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <initializer_list>
#include <string_view>
#include <vector>

#include "syntax.h"

namespace stanly {
TEST_CASE("format lang<...>::firstorder", "[format]") {
  auto fformat = [](auto t) { return std::format("{}", t); };

  SECTION("node") {
    struct syntax_nodes : nodes {
      std::vector<std::pair<firstorder, std::string>> operator()() {
        return {{load{"a", "b", "c"}, std::format("{}(a b c)", type_name<load>)},
                {update{"a", "b", "c"}, std::format("{}(a b c)", type_name<update>)},
                {lit{"a", "b"}, std::format("{}(a b)", type_name<lit>)},
                {alloc{"a", "dict"}, std::format("{}(a dict)", type_name<alloc>)},
                {ref{"a", "b"}, std::format("{}(a b)", type_name<ref>)}};
      }
    };
    auto [node, str] = GENERATE(from_range(syntax_nodes{}()));
    CHECK(visit(fformat, node) == str);    // formatted as choice of a variant
    CHECK(fformat(node) == "inj-" + str);  // formatted as variant
  }
  SECTION("handle node") {
    struct syntax_nodes_handle : packed_nodes {
      std::vector<std::pair<firstorder, std::string>> operator()() {
        return {{load{0_i, 1_i, 2_i}, std::format("{}(0 1 2)", type_name<load>)},
                {update{3_i, 4_i, 5_i}, std::format("{}(3 4 5)", type_name<update>)},
                {lit{6_i, 7_i}, std::format("{}(6 7)", type_name<lit>)},
                {alloc{8_i, 9_i}, std::format("{}(8 9)", type_name<alloc>)},
                {ref{10_i, 11_i}, std::format("{}(10 11)", type_name<ref>)}};
      }
    };
    auto [node, str] = GENERATE(from_range(syntax_nodes_handle{}()));
    CHECK(std::visit(fformat, node) == str);
    CHECK(fformat(node) == "inj-" + str);
  }
  SECTION("std::vector<firstorder>") {
    struct syntax_node_vectors : nodes {
      std::vector<std::pair<std::vector<firstorder>, std::string>> operator()() {
        return {{{load{"a", "b", "c"}, lit{"a", "b"}},
                 std::format("[inj-{}(a b c), inj-{}(a b)]", type_name<load>, type_name<lit>)},
                {{ref{"a", "b"}, alloc{"a", "top"}},
                 std::format("[inj-{}(a b), inj-{}(a top)]", type_name<ref>, type_name<alloc>)},
                {{}, "[]"}};
      }
    };
    auto [node, str] = GENERATE(from_range(syntax_node_vectors{}()));
    CHECK(fformat(node) == str);
  }
  SECTION("std::unordered_map<std::string_view, node>") {
    struct maps : nodes {
      std::vector<std::pair<std::unordered_map<std::string_view, firstorder>, std::string>>
      operator()() {
        return {{{{"2", alloc{"c", "top"}}, {"0", lit{"a", "b"}}, {"3", update{"a", "b", "c"}}},
                 std::format("{}2: inj-{}(c top), 3: inj-{}(a b c), 0: inj-{}(a b){}", "{",
                             type_name<alloc>, type_name<update>, type_name<lit>, "}")},
                {{}, "{}"}};
      }
    };
    auto [map, str] = GENERATE(from_range(maps{}()));
    CHECK(fformat(map) == str);
  };
  SECTION("handle std::vector, std::unordered_map") {
    struct items : packed_nodes {
      std::pair<std::unordered_map<handle, firstorder>, std::string> map = {
          {{0_i, alloc{1_i, 2_i}}, {3_i, lit{3_i, 4_i}}},
          {std::format("{}3: inj-{}(3 4), 0: inj-{}(1 2){}", "{", type_name<lit>, type_name<alloc>,
                       "}")}};
      std::pair<std::vector<firstorder>, std::string> vec = {
          {update{5_i, 6_i, 7_i}, lit{8_i, 9_i}},
          std::format("[inj-{}(5 6 7), inj-{}(8 9)]", type_name<update>, type_name<lit>)};
    };
    auto [map, str] = items{}.map;
    CHECK(fformat(map) == str);
    auto [vec, str_] = items{}.vec;
    CHECK(fformat(vec) == str_);
  }
}

namespace detail {
struct static_assertions_string_view : nodes {
  static_assert(syntax_node<load> && syntax_node<const lit &> && syntax_node<lit &&>);
  static_assert(syntax<firstorder> && syntax<firstorder &&> && syntax<const firstorder &> &&
                syntax<const firstorder &&>);
  static_assert(requires(firstorder node) { std::cout << node; });
  static_assert(requires(update update) { std::cout << update; });
};
struct static_assertions_handle : packed_nodes {
  static_assert(syntax_node<load> && syntax_node<const lit &> && syntax_node<lit &&>);
  static_assert(syntax<firstorder> && syntax<firstorder &&> && syntax<const firstorder &> &&
                syntax<const firstorder &&>);
  static_assert(requires(firstorder node) { std::cout << node; });
  static_assert(requires(update update) { std::cout << update; });
};
static_assert(packed_syntax<packed_nodes::firstorder>);
static_assert(packed_syntax<const packed_nodes::firstorder &>);
}  // namespace detail

}  // namespace stanly

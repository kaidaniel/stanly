#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <initializer_list>
#include <string_view>
#include <vector>

#include "syntax.h"

namespace stanly {
struct test_node {};
template <>
struct is_syntax_node<test_node> {
  constexpr static bool value = true;
};
using test_variant = std::variant<test_node>;
}  // namespace stanly

namespace stanly {
TEST_CASE("format lang<...>::node", "[format]") {
  auto fformat = [](auto &&t) { return std::format("{}", t); };

  SECTION("empty node") {
    CHECK(fformat(test_node{}) == "test_node()");
    CHECK(fformat(test_variant{test_node{}}) == "inj-test_node()");
  }
  SECTION("node") {
    struct syntax_nodes : lang<std::string_view> {
      std::vector<std::pair<node, std::string>> operator()() {
        return {{load{"a", "b", "c"}, std::format("{}(a b c)", type_name<load>)},
                {update{"a", "b", "c"}, std::format("{}(a b c)", type_name<update>)},
                {lit{"a", "b"}, std::format("{}(a b)", type_name<lit>)},
                {alloc{"a", "dict"}, std::format("{}(a dict)", type_name<alloc>)},
                {ref{"a", "b"}, std::format("{}(a b)", type_name<ref>)}};
      }
    };
    auto [node, str] = GENERATE(from_range(syntax_nodes{}()));
    CHECK(std::visit(fformat, node) == str);  // formatted as choice of a variant
    CHECK(fformat(node) == "inj-" + str);     // formatted as variant
  }
  SECTION("idx node") {
    struct syntax_nodes_idx : lang<idx> {
      std::vector<std::pair<node, std::string>> operator()() {
        return {{load{idx{0}, idx{1}, idx{2}}, std::format("{}(0 1 2)", type_name<load>)},
                {update{idx{3}, idx{4}, idx{5}}, std::format("{}(3 4 5)", type_name<update>)},
                {lit{idx{6}, idx{7}}, std::format("{}(6 7)", type_name<lit>)},
                {alloc{idx{8}, idx{9}}, std::format("{}(8 9)", type_name<alloc>)},
                {ref{idx{10}, idx{11}}, std::format("{}(10 11)", type_name<ref>)}};
      }
    };
    auto [node, str] = GENERATE(from_range(syntax_nodes_idx{}()));
    CHECK(std::visit(fformat, node) == str);
    CHECK(fformat(node) == "inj-" + str);
  }
  SECTION("std::vector<node>") {
    struct syntax_node_vectors : lang<std::string_view> {
      std::vector<std::pair<std::vector<node>, std::string>> operator()() {
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
    struct maps : lang<std::string_view> {
      std::vector<std::pair<std::unordered_map<std::string_view, node>, std::string>> operator()() {
        return {{{{"2", alloc{"c", "top"}}, {"0", lit{"a", "b"}}, {"3", update{"a", "b", "c"}}},
                 std::format("{}0: inj-{}(a b), 3: inj-{}(a b c), 2: inj-{}(c top){}", "{",
                             type_name<lit>, type_name<update>, type_name<alloc>, "}")},
                {{}, "{}"}};
      }
    };
    auto [map, str] = GENERATE(from_range(maps{}()));
    CHECK(fformat(map) == str);
  };
  SECTION("idx std::vector, std::unordered_map") {
    struct items : lang<idx> {
      std::pair<std::unordered_map<idx, node>, std::string> map = {
          {{idx{0}, alloc{idx{1}, idx{2}}}, {idx{3}, lit{idx{3}, idx{4}}}},
          {std::format("{}0: inj-{}(1 2), 3: inj-{}(3 4){}", "{", type_name<alloc>, type_name<lit>,
                       "}")}};
      std::pair<std::vector<node>, std::string> vec = {
          {update{idx{5}, idx{6}, idx{7}}, lit{idx{8}, idx{9}}},
          std::format("[inj-{}(5 6 7), inj-{}(8 9)]", type_name<update>, type_name<lit>)};
    };
    auto [map, str] = items{}.map;
    CHECK(fformat(map) == str);
    auto [vec, str_] = items{}.vec;
    CHECK(fformat(vec) == str_);
  }
}
auto &operator<<(auto &s, const formatted_type auto &x) { return s << std::format("{}", x); }
namespace detail {
struct static_assertions_string_view : lang<std::string_view> {
  static_assert(all<is_syntax_node, std::variant<load, const lit &, lit &&>>);
  static_assert(
      all<is_syntax, std::variant<node, node &&, const node, const node &, const node &&>>);
  static_assert(requires(node node) { std::cout << node; });
  static_assert(requires(update update) { std::cout << update; });
};
struct static_assertions_idx : lang<idx> {
  static_assert(all<is_syntax_node, std::variant<load, const lit &, lit &&>>);
  static_assert(
      all<is_syntax, std::variant<node, node &&, const node, const node &, const node &&>>);
  static_assert(requires(node node) { std::cout << node; });
  static_assert(requires(update update) { std::cout << update; });
};
static_assert(packed_syntax<lang<idx>::node>);
static_assert(packed_syntax<const lang<idx>::node &>);
}  // namespace detail

}  // namespace stanly

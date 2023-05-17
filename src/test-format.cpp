#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <initializer_list>
#include <string_view>
#include <vector>

#include "firstorder-syntax.h"

namespace stanly::firstorder {
TEST_CASE("format firstorder::syntax<...>::node", "[format]") {
  constexpr auto fformat = [](auto&& n) { return std::format("{}", n); };

  SECTION("node") {
    struct syntax_nodes : syntax<std::string_view> {
      std::vector<std::pair<node, std::string>> value = {
          {load{"a", "b", "c"}, "load(a b c)"}, {store{"a", "b", "c"}, "store(a b c)"},
          {text{"a", "b"}, "text(a b)"},        {record{"a", {"b", "c"}}, "record(a [b, c])"},
          {ref{"a", "b"}, "ref(a b)"},          {top{"a", "b"}, "top(a b)"}};
    };
    auto [node, str] = GENERATE(from_range(syntax_nodes{}.value));
    CHECK(std::visit(fformat, node) == str);  // formatted as choice of a variant
    CHECK(fformat(node) == "inj-" + str);     // formatted as variant
  }
  SECTION("std::vector<node>") {
    struct syntax_node_vectors : syntax<std::string_view> {
      std::vector<std::pair<std::vector<node>, std::string>> value = {
          {{load{"a", "b", "c"}, text{"a", "b"}}, "[inj-load(a b c), inj-text(a b)]"},
          {{ref{"a", "b"}, top{"a", "b"}}, "[inj-ref(a b), inj-top(a b)]"},
          {{}, "[]"}};
    };
    auto [node, str] = GENERATE(from_range(syntax_node_vectors{}.value));
    CHECK(fformat(node) == str);
  }
}

}  // namespace stanly::firstorder

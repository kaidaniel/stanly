#include <catch2/catch_test_macros.hpp>
#include <string_view>
#include <vector>

#include "firstorder-format.h"
#include "firstorder-syntax.h"

using stx = stanly::firstorder::syntax<std::string_view>;
using node = stx::node;

TEST_CASE("firstorder syntax nodes", "[format]") {
  REQUIRE(std::format("{}", stx::load_field{"a", "b", "c"}) == "load_field(a b c)");
  REQUIRE(std::format("{}", stx::set_field{"a", "b", "c"}) == "set_field(a b c)");
  REQUIRE(std::format("{}", stx::load_text{"a", "b"}) == "load_text(a b)");
  REQUIRE(std::format("{}", stx::load_record{"a", {"b", "c"}}) == "load_record(a [b, c])");
  REQUIRE(std::format("{}", stx::load_var{"a", "b"}) == "load_var(a b)");
  REQUIRE(std::format("{}", stx::load_top{"a", "b"}) == "load_top(a b)");
}

TEST_CASE("firstorder syntax variant", "[format]") {
  REQUIRE(std::format("{}", node{stx::load_field{"a", "b", "c"}}) == "inj-load_field(a b c)");
  REQUIRE(std::format("{}", node{stx::set_field{"a", "b", "c"}}) == "inj-set_field(a b c)");
  REQUIRE(std::format("{}", node{stx::load_text{"a", "b"}}) == "inj-load_text(a b)");
  REQUIRE(std::format("{}", node{stx::load_var{"a", "b"}}) == "inj-load_var(a b)");
  REQUIRE(std::format("{}", node{stx::load_top{"a", "b"}}) == "inj-load_top(a b)");
  REQUIRE(std::format("{}", node{stx::load_record{"a", {"b", "c"}}}) ==
          "inj-load_record(a [b, c])");
}

TEST_CASE("std::vector", "[format]") {
  REQUIRE(std::format("{}", std::vector{"a", "b", "c"}) == "[a, b, c]");
  REQUIRE(std::format("{}", std::vector{'a', 'b', 'c'}) == "[a, b, c]");
  REQUIRE(std::format("{}", std::vector{1, 2, 3, 4, 5}) == "[1, 2, 3, 4, 5]");
  REQUIRE(std::format("{}", std::vector{node{stx::load_field{"a", "b", "c"}},
                                        node{stx::load_text{"a", "b"}}}) ==
          "[inj-load_field(a b c), inj-load_text(a b)]");
}

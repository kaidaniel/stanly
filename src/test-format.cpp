#include <catch2/catch_test_macros.hpp>
#include <string_view>
#include <vector>

#include "firstorder-syntax.h"
#include "stanly-format.h"

using stx = stanly::firstorder::syntax<std::string_view>;
using load_var = stx::load_var;
using load_text = stx::load_text;
using load_top = stx::load_top;
using load_record = stx::load_record;
using load_field = stx::load_field;
using set_field = stx::set_field;
using node = stx::node;
using std::format;
using std::vector;

TEST_CASE("format firstorder syntax nodes", "[format]") {
  REQUIRE(format("{}", load_field{"a", "b", "c"}) == "load_field(a b c)");
  REQUIRE(format("{}", set_field{"a", "b", "c"}) == "set_field(a b c)");
  REQUIRE(format("{}", load_text{"a", "b"}) == "load_text(a b)");
  REQUIRE(format("{}", load_record{"a", {"b", "c"}}) == "load_record(a [b, c])");
  REQUIRE(format("{}", load_var{"a", "b"}) == "load_var(a b)");
  REQUIRE(format("{}", load_top{"a", "b"}) == "load_top(a b)");
}

TEST_CASE("format firstorder syntax variant", "[format]") {
  REQUIRE(format("{}", node{load_field{"a", "b", "c"}}) == "inj-load_field(a b c)");
  REQUIRE(format("{}", node{set_field{"a", "b", "c"}}) == "inj-set_field(a b c)");
  REQUIRE(format("{}", node{load_text{"a", "b"}}) == "inj-load_text(a b)");
  REQUIRE(format("{}", node{load_var{"a", "b"}}) == "inj-load_var(a b)");
  REQUIRE(format("{}", node{load_top{"a", "b"}}) == "inj-load_top(a b)");
  REQUIRE(format("{}", node{load_record{"a", {"b", "c"}}}) == "inj-load_record(a [b, c])");
}

TEST_CASE("format std::vector", "[format]") {
  REQUIRE(format("{}", vector{"a", "b", "c"}) == "[a, b, c]");
  REQUIRE(format("{}", vector{'a', 'b', 'c'}) == "[a, b, c]");
  REQUIRE(format("{}", vector{1, 2, 3, 4, 5}) == "[1, 2, 3, 4, 5]");
  REQUIRE(format("{}", vector{node{load_field{"a", "b", "c"}}, node{load_text{"a", "b"}}}) ==
          "[inj-load_field(a b c), inj-load_text(a b)]");
}

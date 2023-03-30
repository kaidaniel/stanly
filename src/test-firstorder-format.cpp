#include <fmt/core.h>

#include <catch2/catch_test_macros.hpp>
#include <string_view>

#include "firstorder-format.h"
#include "firstorder-syntax.h"

using stx = stanly::firstorder::syntax<std::string_view>;
using node = stx::node;
using fmt::format;

TEST_CASE("firstorder syntax nodes correctly formatted", "[format]") {
  REQUIRE(format("{}", stx::load_field{"a", "b", "c"}) == "load_field(a b c)");
  REQUIRE(format("{}", stx::set_field{"a", "b", "c"}) == "set_field(a b c)");
  REQUIRE(format("{}", stx::load_text{"a", "b"}) == "load_text(a b)");
  REQUIRE(format("{}", stx::load_record{"a", {"b", "c"}}) == "load_record(a [b, c])");
  REQUIRE(format("{}", stx::load_var{"a", "b"}) == "load_var(a b)");
  REQUIRE(format("{}", stx::load_top{"a", "b"}) == "load_top(a b)");
}

TEST_CASE("firstorder syntax variant correctly formatted", "[format]") {
  REQUIRE(format("{}", node{stx::load_field{"a", "b", "c"}}) == "variant(load_field(a b c))");
  REQUIRE(format("{}", node{stx::set_field{"a", "b", "c"}}) == "variant(set_field(a b c))");
  REQUIRE(format("{}", node{stx::load_text{"a", "b"}}) == "variant(load_text(a b))");
  REQUIRE(format("{}", node{stx::load_var{"a", "b"}}) == "variant(load_var(a b))");
  REQUIRE(format("{}", node{stx::load_top{"a", "b"}}) == "variant(load_top(a b))");
}

class syntax_range {
  int state{0};

 public:
  node generate() {
    ++state;
    if (state == 1) {
      return node{stx::load_field{"a", "b", "c"}};
    }
    if (state == 2) {
      return node{stx::set_field{"a", "b", "c"}};
    }
    if (state == 3) {
      return node{stx::load_top{"a", "b"}};
    }
    if (state == 4) {
      return node{};
    }
    throw "unreachable";
  }
  [[nodiscard]] bool is_exhausted() const { return state > 3; }
};

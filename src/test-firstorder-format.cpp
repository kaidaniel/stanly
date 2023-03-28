#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/std.h>
#include <fmt/ranges.h>
#include <catch2/catch_test_macros.hpp>
#include <string_view>

#include "firstorder-syntax.h"
#include "firstorder-format.h"

using stx = stanly::firstorder::syntax<std::string_view>;
using fmt::format;

TEST_CASE("firstorder syntax nodes correctly formatted", "[format]") {
  REQUIRE(format("{}", stx::load_field{"a", "b", "c"}) == "(load_field a b c)");
  REQUIRE(format("{}", stx::set_field{"a", "b", "c"}) == "(set_field a b c)");
  REQUIRE(format("{}", stx::load_text{"a", "b"}) == "(load_text a b)");
  REQUIRE(format("{}", stx::load_record{"a", {"b", "c"}}) == "(load_record a [b, c])");
  REQUIRE(format("{}", stx::load_var{"a", "b"}) == "(load_var a b)");
  REQUIRE(format("{}", stx::load_top{"a", "b"}) == "(load_top a b)");
}

#include <catch2/catch_test_macros.hpp>
#include <string_view>
#include <vector>

#include "firstorder-syntax.h"
#include "stanly-format.h"

using syn = stanly::firstorder::syntax<std::string_view>;

TEST_CASE("format firstorder syntax nodes", "[format]") {
  REQUIRE(std::format("{}", syn::load{"a", "b", "c"}) == "load(a b c)");
  REQUIRE(std::format("{}", syn::store{"a", "b", "c"}) == "store(a b c)");
  REQUIRE(std::format("{}", syn::text{"a", "b"}) == "text(a b)");
  REQUIRE(std::format("{}", syn::record{"a", {"b", "c"}}) == "record(a [b, c])");
  REQUIRE(std::format("{}", syn::ref{"a", "b"}) == "ref(a b)");
  REQUIRE(std::format("{}", syn::top{"a", "b"}) == "top(a b)");
}

TEST_CASE("format firstorder syntax variant", "[format]") {
  REQUIRE(std::format("{}", syn::node{syn::load{"a", "b", "c"}}) == "inj-load(a b c)");
  REQUIRE(std::format("{}", syn::node{syn::store{"a", "b", "c"}}) == "inj-store(a b c)");
  REQUIRE(std::format("{}", syn::node{syn::text{"a", "b"}}) == "inj-text(a b)");
  REQUIRE(std::format("{}", syn::node{syn::ref{"a", "b"}}) == "inj-ref(a b)");
  REQUIRE(std::format("{}", syn::node{syn::top{"a", "b"}}) == "inj-top(a b)");
  REQUIRE(std::format("{}", syn::node{syn::record{"a", {"b", "c"}}}) == "inj-record(a [b, c])");
}

TEST_CASE("format std::vector", "[format]") {
  REQUIRE(std::format("{}", std::vector{"a", "b", "c"}) == "[a, b, c]");
  REQUIRE(std::format("{}", std::vector{'a', 'b', 'c'}) == "[a, b, c]");
  REQUIRE(std::format("{}", std::vector{1, 2, 3, 4, 5}) == "[1, 2, 3, 4, 5]");
  REQUIRE(format("{}", std::vector{syn::node{syn::load{"a", "b", "c"}},
                                   syn::node{syn::text{"a", "b"}}}) ==
          "[inj-load(a b c), inj-text(a b)]");
}

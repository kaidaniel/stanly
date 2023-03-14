#include "metaprogramming.h"
#include <catch2/catch_test_macros.hpp>

namespace stanly::metaprogramming::test {
struct s {};
using a = s;
class c__d {};
template <class T> struct tt { using type = T; };
struct m {
  struct member {};
};
template <class A, class B> struct qq {};

TEST_CASE("type_name", "[metaprogramming]") {
  REQUIRE(type_name<s> == "s");
  REQUIRE(type_name<a> == "s");
  REQUIRE(type_name<tt<s>::type> == "s");
  REQUIRE(type_name<tt<s>> == "tt<s>");
  REQUIRE(type_name<m::member> == "member");
  REQUIRE(type_name<tt<tt<c__d>>> == "tt<tt<c__d>>");
  REQUIRE(type_name<tt<tt<tt<c__d>>>> == "tt<tt<tt<c__d>>>");
  REQUIRE(type_name<qq<tt<s>, tt<s>>> == "qq<tt<s>,tt<s>>");
};
} // namespace stanly::metaprogramming::test

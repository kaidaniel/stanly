#include "metaprogramming.h"
#include <catch2/catch_test_macros.hpp>
#include <tuple>
#include <type_traits>

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

struct zero {};
struct one {
  int i;
};
struct two {
  int i, j;
};
struct three {
  int i, j, k;
};
struct four {
  int i, j, k, l;
};
TEST_CASE("struct_to_tpl", "[metaprogramming]") {
  REQUIRE(struct_to_tpl(zero{}) == std::make_tuple());
  REQUIRE(struct_to_tpl(one{1}) == std::make_tuple(1));
  REQUIRE(struct_to_tpl(two{1, 2}) == std::make_tuple(1, 2));
  REQUIRE(struct_to_tpl(three{1, 2, 3}) == std::make_tuple(1, 2, 3));
  REQUIRE(struct_to_tpl(four{1, 2, 3, 4}) == std::make_tuple(1, 2, 3, 4));
};
static_assert(std::is_same_v<decltype(struct_to_tpl(zero{})), std::tuple<>>);
static_assert(std::is_same_v<decltype(struct_to_tpl(one{1})), std::tuple<int>>);
static_assert(
    std::is_same_v<decltype(struct_to_tpl(two{1, 2})), std::tuple<int, int>>);
static_assert(
    std::is_same_v<
        decltype(struct_to_tpl(three{1, 2, 3})), std::tuple<int, int, int>>);
static_assert(std::is_same_v<
              decltype(struct_to_tpl(four{1, 2, 3, 4})),
              std::tuple<int, int, int, int>>);

} // namespace stanly::metaprogramming::test

#include <catch2/catch_test_macros.hpp>

#include "stanly-utils.h"

namespace stanly {

template <class T>
struct x {
  struct a {
    bool operator==(const a& other) const = default;
  };
  struct b {
    T i, j;
    bool operator==(const b& other) const = default;
  };
  struct c {
    T i, j;
    bool operator==(const c& other) const = default;
  };
};

using variant1 = std::variant<x<int>::a, x<int>::b>;
using variant2 = std::variant<x<float>::a, x<float>::b>;
using same_tuple_sizes_different_type_names = std::variant<x<float>::a, x<float>::c>;
using same_type_names_different_tuple_sizes = std::variant<struct a, struct b>;

static_assert(variants_with_same_type_names<variant1, variant2>);
static_assert(variants_with_same_type_names<variant1, same_type_names_different_tuple_sizes>);
static_assert(!variants_with_same_type_names<variant1, same_tuple_sizes_different_type_names>);

static_assert(variants_with_same_tuple_sizes<variant1, variant2>);
static_assert(variants_with_same_tuple_sizes<variant1, same_tuple_sizes_different_type_names>);
static_assert(!variants_with_same_tuple_sizes<variant1, same_type_names_different_tuple_sizes>);

static_assert(std::same_as<x<int>::a, search_same_name_t<x<int>::a, variant1>>);
static_assert(std::same_as<x<int>::a, search_same_name_t<x<struct anything>::a, variant1>>);
static_assert(std::same_as<x<int>::a, search_same_name_t<struct a, variant1>>);
static_assert(!search_same_name_t<struct d, variant1>::value);

TEST_CASE("utils", "[utils]") {
  auto m_mapper =
      map_members<search_same_name_t, variant2>([](auto&& x) -> float { return 0.5 + x; });
  auto v_mapper = map_to_same_name<variant1, variant2>([](auto&& x) -> float { return x + 0.5; });
  REQUIRE(m_mapper(x<int>::b{1, 2}) == x<float>::b{1.5, 2.5});
  REQUIRE(m_mapper(x<int>::a{}) == x<float>::a{});
  REQUIRE(v_mapper(variant1{x<int>::b{1, 2}}) == variant2{x<float>::b{1.5, 2.5}});
  REQUIRE(v_mapper(variant1{x<int>::a{}}) == variant2{x<float>::a{}});
}
}  // namespace stanly
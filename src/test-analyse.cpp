#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <string_view>
#include <vector>

#include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "firstorder-syntax.h"
#include "syntax.h"

namespace stanly::firstorder {
// TODO: replace with actual abstract domain in src/firstorder-syntax.h
// clang-format off
template<class Repr>
struct domain {
  struct str { Repr val; };
  struct record { Repr field1; Repr field2; };
  struct recordv { Repr field1; Repr field2; };
  struct top{};
  struct bottom{};
  using object = std::variant<str, record, recordv, top, bottom>;
  using env = std::unordered_map<Repr, object>;
 };
 std::ostream& operator<<(std::ostream& os, std::vector<domain<std::size_t>::env> vec) { 
  return (os << std::format("{}", vec));
  }
}
namespace stanly{
  template<class T>
  requires contains<firstorder::domain<std::size_t>::object, std::decay_t<T>>
  struct is_syntax_node<T> { constexpr static bool value = true; };

namespace firstorder{
// clang-format on
auto const analyse = [](auto&&) -> domain<std::size_t>::env { return domain<std::size_t>::env{}; };
struct programs : syntax<idx> {
  std::vector<std::vector<node>> operator()() {
    return {{text{idx{0}, idx{1}}, record{idx{2}, idx{100}}, store{idx{2}, idx{3}, idx{0}},
             store{idx{2}, idx{4}, idx{0}}},
            {record{idx{0}, idx{101}}, load{idx{3}, idx{0}, idx{1}}, load{idx{4}, idx{0}, idx{5}},
             ref{idx{6}, idx{0}}},
            {top{idx{0}, idx{1}}, store{idx{0}, idx{2}, idx{0}}, load{idx{3}, idx{0}, idx{4}}}};
  }
};
struct bindings : domain<std::size_t> {
  std::vector<env> operator()() {
    return {{{0, str{1}}, {2, record{3, 4}}},
            {{0, record{1, 2}}, {3, top{}}, {4, bottom{}}, {6, bottom{}}},
            {{0, recordv{2, 4}}, {3, top{}}}};
  }
};
TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  namespace m = Catch::Matchers;
  namespace views = std::views;
  CHECK_THAT(bindings{}(), m::RangeEquals(views::transform(programs{}(), analyse)));
}
}  // namespace firstorder
}  // namespace stanly
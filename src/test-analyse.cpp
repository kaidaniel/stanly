#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <string_view>
#include <vector>

#include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "firstorder-analyse.h"
#include "firstorder-syntax.h"
#include "syntax.h"

namespace stanly::firstorder {
struct programs : syntax<idx> {
  std::vector<std::vector<node>> operator()() {
    return {{text{idx{0}, idx{1}}, alloc{idx{2}, idx{100}}, update{idx{2}, idx{3}, idx{0}},
             update{idx{2}, idx{4}, idx{0}}},
            {alloc{idx{0}, idx{101}}, load{idx{3}, idx{0}, idx{1}}, load{idx{4}, idx{0}, idx{5}},
             ref{idx{6}, idx{0}}},
            {alloc{idx{0}, idx{9}}, update{idx{0}, idx{2}, idx{0}}, load{idx{3}, idx{0}, idx{4}}}};
  }
};
struct bindings : domain<std::size_t> {
  std::vector<env> operator()() {
    return {{{0, str{1}}, {2, record{3, 4}}},
            {{0, record{1, 2}}, {3, top{}}, {4, bottom{}}, {6, bottom{}}},
            {{0, recordv{2, 4}}, {3, top{}}}};
  }
};

TEST_CASE("analyse firstorder programs", "[firstorder][.analyse]") {
  CHECK_THAT(bindings{}(), Catch::Matchers::RangeEquals(vw::transform(programs{}(), analyse)));
}
}  // namespace stanly::firstorder

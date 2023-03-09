#include "iterator.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

#include <iterator>
#include <linux/limits.h>
#include <memory>
#include <ranges>

namespace stanly::iterator::test {
template <class T, class Return> constexpr bool check_concepts() {
  static_assert(std::input_iterator<std::ranges::iterator_t<T>>);
  static_assert(
      std::same_as<std::ranges::iterator_t<T>, inpt_iterator<Return>>);
  static_assert(std::input_iterator<inpt_iterator<Return>>);
  static_assert(std::ranges::range<inpt_range<Return>>);
  static_assert(std::ranges::range<T>);
  static_assert(std::ranges::input_range<T>);
  return true;
};
bool range_was_destroyed = false;
struct count_to_five {
public:
  int count() {
    number_++;
    return number_;
  }
  [[nodiscard]] bool over_five() const { return number_ >= 5; }
  int number_{0};
  ~count_to_five() {
    number_ = -100;
    range_was_destroyed = true;
  }
};
static_assert(check_concepts<inpt_range<int>, int>());

TEST_CASE("count_to_five", "[iterator]") {
  REQUIRE(not range_was_destroyed);
  {
    int number = 0;
    for (inpt_range ct5{&count_to_five::count, &count_to_five::over_five};
         const auto &i : ct5) {
      REQUIRE(i == number);
      number++;
      REQUIRE(not range_was_destroyed);
    };
    REQUIRE(number == 5);
    REQUIRE(range_was_destroyed);
    inpt_range x{&count_to_five::count, &count_to_five::over_five};
    range_was_destroyed = false;
    number = 0;
    for (const auto &i : x) {
      REQUIRE(i == number);
      number++;
    };
    REQUIRE(not range_was_destroyed);
  }
  REQUIRE(range_was_destroyed);
};

} // namespace stanly::iterator::test

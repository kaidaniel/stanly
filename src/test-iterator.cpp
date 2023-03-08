#include "iterator.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

#include <iterator>
#include <ranges>
#include <memory>

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
bool range_subclass_was_destroyed = false;
class count_to_five : public inpt_range<int> {
  int count() {
    number_++;
    return number_;
  }
  [[nodiscard]] bool over_five() const { return number_ >= 5; }
  
public:
  count_to_five()
      : inpt_range(&count_to_five::count, &count_to_five::over_five) {}
  int number_{0};
  ~count_to_five() { number_ = -100; range_subclass_was_destroyed = true; }
};
static_assert(check_concepts<count_to_five, int>());

TEST_CASE("count_to_five", "[iterator]") {
  {
    count_to_five ct5{};
    int number = 0;
    for (const auto &i : ct5) {
      REQUIRE(i == number);
      number++;
    };
    REQUIRE(ct5.number_ == 5);
  }
  REQUIRE(range_subclass_was_destroyed);
};

TEST_CASE("unique_ptr conversion", "[iterator]") {
  range_subclass_was_destroyed = false;
  {
    std::unique_ptr<inpt_range<int>> range_uptr = std::make_unique<count_to_five>();
    int number = 0;
    for (const auto &i : *range_uptr) {
      REQUIRE(i == number);
      number++;
    };
    REQUIRE(static_cast<count_to_five*>(range_uptr.get())->number_ == 5);
    REQUIRE(not range_subclass_was_destroyed);
  }
  REQUIRE(range_subclass_was_destroyed);
};

TEST_CASE("unique_ptr conversion by move", "[iterator]") {
  range_subclass_was_destroyed = false;
  {
    std::unique_ptr<inpt_range<int>> range_uptr = []{
      auto uptr = std::make_unique<count_to_five>();
      return std::move(uptr);
    }();
    int number = 0;
    for (const auto &i : *range_uptr) {
      REQUIRE(i == number);
      number++;
    };
    REQUIRE(static_cast<count_to_five*>(range_uptr.get())->number_ == 5);
    REQUIRE(not range_subclass_was_destroyed);
  }
  REQUIRE(range_subclass_was_destroyed); // TODO: fails. inpt_range needs to call destructor of subclass.
};


      

} // namespace stanly::iterator::test

#include "iterator.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

#include <ranges>
#include <iterator>

namespace stanly::iterator::test{
template<class T, class Return>
constexpr bool check_concepts() {
  static_assert(std::input_iterator<std::ranges::iterator_t<T>>);
  static_assert(std::same_as<std::ranges::iterator_t<T>, input_iter<T, Return>>);
  static_assert(std::input_iterator<input_iter<T, Return>>);
  static_assert(std::ranges::range<input_range_adaptor<T, Return>>);
  static_assert(std::ranges::range<T>);
  static_assert(std::ranges::input_range<T>);
  return true;
};
class count_to_five : public input_range_adaptor<count_to_five, int> {
    int count() { number_++; return number_; }
    [[nodiscard]] bool over_five() { return number_ >= 5; }
    int number_{0};
    public:
    count_to_five() : input_range_adaptor(&count_to_five::count, &count_to_five::over_five) {}
    int& number(){return number_;}
};
static_assert(check_concepts<count_to_five, int>());

TEST_CASE("count_to_five", "[iterator]") {
  count_to_five ct5{};
  int number = 0;
  for(const auto& i : ct5){
    REQUIRE(i == number);
    number++;

  };
  REQUIRE(ct5.number() == 5);
};
}



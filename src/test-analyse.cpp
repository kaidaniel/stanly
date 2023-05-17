#include <catch2/catch_test_macros.hpp>
#include <string_view>
#include <vector>

#include "firstorder-syntax.h"

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
// clang-format on
domain<int>::env analyse(auto&&) { return domain<int>::env{}; }
struct programs_s : syntax<int> {
  std::vector<std::vector<node>> value = {
      {text{0, 1}, record{2, 100}, store{2, 3, 0}, store{2, 4, 0}},
      {record{0, 101}, load{3, 0, 1}, load{4, 0, 5}, ref{6, 0}},
      {top{0, 1}, store{0, 2, 0}, load{3, 0, 4}}};
};
struct bindings_s : domain<int> {
  std::vector<env> value = {{{0, str{1}}, {2, record{3, 4}}},
                            {{0, record{1, 2}}, {3, top{}}, {4, bottom{}}, {6, bottom{}}},
                            {{0, recordv{2, 4}}, {3, top{}}}};
};
TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  auto programs = programs_s{}.value;
  auto bindings = bindings_s{}.value;
  CHECK(analyse(programs[0]) == bindings[0]);
  CHECK(analyse(programs[1]) == bindings[1]);
  CHECK(analyse(programs[2]) == bindings[2]);
}
}  // namespace stanly::firstorder

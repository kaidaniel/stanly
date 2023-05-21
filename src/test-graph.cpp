#include <type_traits>

#include "catch2/catch_test_macros.hpp"
#include "graph.h"
#include "syntax.h"

namespace stanly {
template <class Repr>
struct variants {
  using record = std::conditional_t<std::is_same_v<Repr, std::string_view>,
                                    std::vector<std::string_view>, Repr>;
  struct a {
    Repr i;
    Repr j;
    bool operator==(const a&) const = default;
  };
  struct b {
    Repr i;
    Repr k;
  };
  using variant = std::variant<a, b>;
};
using unpacked_t = variants<std::string_view>;
using t = variants<handle>;
TEST_CASE("graph", "[graph]") {
  auto a1 = unpacked_t::a{"a1i", "a1j"};
  auto a2 = unpacked_t::a{"a2i", "a2j"};
  //  auto b1 = unpacked_t::b{"b1i",  {"b1k1", "b1k2", "b1k3"}};
  auto b1 = unpacked_t::b{"b", "bb"};
  auto parse = [=](std::string_view) -> std::vector<unpacked_t::variant> {
    return {{a1}, {a2}, {b1}};
  };
  auto read_program = []() -> std::string {
    return "test string - this isn't actually a program.";
  };
  static_assert(std::tuple_size_v<decltype(to_tpl(t::b{}))> == 2);
  auto test_graph = graph<t::variant, unpacked_t::variant>{parse, read_program};

  //  REQUIRE(graph.view_syntax()[0] == a1);
}
}  // namespace stanly
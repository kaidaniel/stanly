
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <format>
#include <map>
#include <ranges>
#include <vector>

#include "analyse.h"
#include "domain.h"
#include "syntax.h"

namespace stanly {
using namespace domains;
std::map<handle, std::string_view> variable_names{};
handle operator""_h(const char* str, std::size_t size) {
  static std::map<std::string_view, handle> variable_handles{};
  auto sv = std::string_view{str, size};
  if (!variable_handles.contains(sv)) {
    auto h = handle{variable_handles.size()};
    variable_handles[sv] = h;
    variable_names[h] = sv;
  }
  return variable_handles[sv];
}

class collected_states : public packed_nodes {
  struct result {
    std::vector<firstorder> nodes{};
    state state{};
  };
  std::vector<result> results{result{{}, {}}};
  void add_node(firstorder&& n) {
    results.push_back(results.back());
    results.back().nodes.push_back(n);
  }
  template <class Target>
  void set_key(auto&&... args) {
    results.back().state.set_key<Target>(args...);
  }

 public:
  state& res() { return results.back().state; }
  std::vector<result> operator()() && {
    add_node(alloc{"al"_h, "unknown"_h});
    set_key<scope>("al"_h, addresses{"al"_h});
    set_key<memory>("al"_h, object{{type{"unknown"_h}, data::bottom()}});

    add_node(lit{"lt"_h, "int"_h, "123"_h});
    set_key<scope>("lt"_h, addresses{"lt"_h});
    set_key<memory>("lt"_h, object{{type{"int"_h}, data{constant{"123"_h}}}});

    add_node(update{"al"_h, "f"_h, "lt"_h});
    set_key<memory>("al"_h,
                    object{{type{"unknown"_h},
                            data{record{{top, defined{{{"f"_h, addresses{"lt"_h}}}}, bot}}}}});

    add_node(ref{"rf"_h, "al"_h});
    set_key<scope>("rf"_h, addresses{"al"_h});

    add_node(load{"ld"_h, "al"_h, "f"_h});
    set_key<scope>("ld"_h, addresses{"lt"_h});
    set_key<memory>(
        "al"_h, object{{type{"unknown"_h},
                        data{record{{top, defined{{{"f"_h, addresses{"lt"_h}}}}, used{"f"_h}}}}}});

    add_node(load{"ld"_h, "al"_h, "g"_h});
    set_key<scope>("ld"_h, top);
    set_key<memory>(
        "al"_h,
        object{{type{"unknown"_h},
                data{record{{top, defined{{{"f"_h, addresses{"lt"_h}}}}, used{"f"_h, "g"_h}}}}}});

    add_node(alloc{"al"_h, "dict"_h});
    set_key<scope>("al"_h, addresses{"al"_h});

    add_node(ref{"rf"_h, "al"_h});
    set_key<scope>("rf"_h, addresses{"al"_h});

    return results;
  }
};
TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  auto [program, state] = GENERATE(from_range(collected_states{}()));
  INFO(std::format("variable_names={}\n", variable_names));
  CHECK(analyse(program) == state);
}
}  // namespace stanly
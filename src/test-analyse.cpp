
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <format>
#include <map>
#include <vector>

#include "analyse.h"
#include "domain.h"
#include "syntax.h"

namespace stanly {

using namespace domains;
using namespace syntax;
std::map<handle, std::string_view> handle_names{};
std::map<std::string_view, handle> variable_handles{};
handle operator""_h(const char* str, std::size_t size) {
  auto sv = std::string_view{str, size};
  if (!variable_handles.contains(sv)) {
    auto h = handle{variable_handles.size()};
    variable_handles[sv] = h;
    handle_names[h] = sv;
  }
  return variable_handles[sv];
}
std::string replace_handles_with_names(std::string msg) {
  for (const auto& [handle, name] : handle_names) {
    auto hndl = std::format("{}", handle);
    size_t pos = msg.find(hndl);
    while (pos != std::string::npos) {
      if (msg[pos - 1] != '#') { msg.replace(pos, hndl.length(), name); }
      pos = msg.find(hndl, pos + name.length());
    }
  }
  return msg;
}

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

TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  add_node(alloc{"al"_h, "unknown"_h});
  set_key<scope>("al"_h, addresses{"al"_h});
  set_key<memory>("al"_h, object{{type{"unknown"_h}, data::bottom()}});

  add_node(lit{"lt"_h, "int"_h, "123"_h});
  set_key<scope>("lt"_h, addresses{"lt"_h});
  set_key<memory>("lt"_h, object{{type{"int"_h}, data{constant{"123"_h}}}});

  add_node(update{"al"_h, "f"_h, "lt"_h});
  set_key<memory>(
      "al"_h,
      object{{type{"unknown"_h}, data{record{{top, defined{{{"f"_h, addresses{"lt"_h}}}}, bot}}}}});

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

  auto [program, expected_state] = GENERATE(from_range(results));
  auto observed_state = analyse(program);

  auto msg = std::format(
      "\n"
      "program:\n{}\n\n"
      "expected:\n{}\n\n"
      "observed:\n{}\n\n",
      program, expected_state, observed_state);
  INFO(replace_handles_with_names(msg) << std::format("variable_handles:\n{}", variable_handles));
  bool analysis_inferred_correct_state = expected_state == observed_state;
  CHECK(analysis_inferred_correct_state);
}

}  // namespace stanly

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

const static class handle_pool {
  mutable std::map<handle, std::string_view> handle_to_str{};
  mutable std::map<std::string_view, handle> str_to_handle{};
  handle get_handle(std::string_view sv) const {
    if (!str_to_handle.contains(sv)) {
      auto h = handle{str_to_handle.size()};
      str_to_handle[sv] = h;
      handle_to_str[h] = sv;
    };
    return str_to_handle[sv];
  }

 public:
  friend handle operator""_h(const char* str, std::size_t size);
  [[nodiscard]] const std::map<handle, std::string_view>& handles() const { return handle_to_str; }
  [[nodiscard]] const std::map<std::string_view, handle>& variables() const {
    return str_to_handle;
  }
} handle_pool{};

handle operator""_h(const char* str, std::size_t size) {
  return handle_pool.get_handle(std::string_view{str, size});
}

std::string replace_handles(std::string msg) {
  for (const auto& [handle, name] : handle_pool.handles()) {
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

  const auto& [program, state] = GENERATE(from_range(results));
  INFO(replace_handles(std::format("\nprogram:\n{:lines}", program)));
  INFO(replace_handles(std::format("\nexpected:\n{}", state)));

  auto observed = analyse(program);

  INFO(replace_handles(std::format("\nobserved:\n{}\n\n", observed)));
  INFO(std::format("variable_handles:\n{:lines}", handle_pool.variables()));
  bool analysis_inferred_correct_state = state == observed;
  REQUIRE(analysis_inferred_correct_state);
}

}  // namespace stanly
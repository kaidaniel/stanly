
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

class handle_pool {
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
};

static const handle_pool handle_pool{};

handle operator""_h(const char* str, std::size_t size) {
  return handle_pool.get_handle(std::string_view{str, size});
}

template <class... Ts>
auto map_tpl(auto&& f, std::tuple<Ts...> tpl) {
  return std::apply([&f](Ts... t) { return std::tuple{f(std::forward<Ts>(t))...}; }, tpl);
}

std::string resolve_handles(const firstorder& node,
                            const std::map<handle, std::string_view>& handle_to_str) {
  return std::visit(
      [&]<class T>(const T& n) {
        auto elements_tpl =
            map_tpl([&](const handle& h) { return handle_to_str.at(h); }, to_tpl(n));
        return std::format("{}{}", stanly::type_name<T>, elements_tpl);
      },
      node);
}

std::vector<std::string> resolve_handles(const std::vector<firstorder>& nodes,
                                         const std::map<handle, std::string_view>& handle_to_str) {
  std::vector<std::string> out;
  out.reserve(nodes.size());
  for (const auto& node : nodes) { out.push_back(resolve_handles(node, handle_to_str)); }
  return out;
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
  add_node(alloc{"alloc1"_h, "unknown"_h});
  set_key<scope>("alloc1"_h, addresses{"alloc1"_h});
  set_key<memory>(
      "alloc1"_h,
      object{{type{"unknown"_h}, data{record{{row_var{RowVarEls::Closed}, defined{}, used{}}}}}});

  add_node(lit{"lit1"_h, "int"_h, "123"_h});
  set_key<scope>("lit1"_h, addresses{"123"_h});
  set_key<memory>("123"_h, object{{type{"int"_h}, data{constant{"123"_h}}}});

  add_node(lit{"field1"_h, "str"_h, "field1_val"_h});
  set_key<scope>("field1"_h, addresses{"field1_val"_h});
  set_key<memory>("field1_val"_h, object{{type{"str"_h}, data{constant{"field1_val"_h}}}});

  add_node(update{"alloc1"_h, "field1"_h, "lit1"_h});
  set_key<memory>(
      "alloc1"_h,
      object{{type{"unknown"_h},
              data{record{{row_var{RowVarEls::Closed},
                           defined{{{"field1_val"_h, addresses{"123"_h}}}}, used{}}}}}});

  add_node(ref{"ref1"_h, "alloc1"_h});
  set_key<scope>("ref1"_h, addresses{"alloc1"_h});

  add_node(load{"load1"_h, "alloc1"_h, "field1"_h});
  set_key<scope>("load1"_h, addresses{"123"_h});
  set_key<memory>(
      "alloc1"_h,
      object{{type{"unknown"_h}, data{record{{bot, defined{{{"field1_val"_h, addresses{"123"_h}}}},
                                              used{"field1_val"_h}}}}}});

  add_node(lit{"field2"_h, "str"_h, "field2_val"_h});
  set_key<scope>("field2"_h, addresses{"field2_val"_h});
  set_key<memory>("field2_val"_h, object{{type{"str"_h}, data{constant{"field2_val"_h}}}});

  add_node(alloc{"alloc1"_h, "dict"_h});
  set_key<scope>("alloc1"_h, addresses{"alloc1"_h});
  set_key<memory>("alloc1"_h, object{{type{"dict"_h}, data{record{{bot, defined{}, used{}}}}}});

  add_node(ref{"ref1"_h, "alloc1"_h});
  set_key<scope>("ref1"_h, addresses{"alloc1"_h});

  add_node(load{"load1"_h, "alloc1"_h, "field2"_h});
  // row var is closed: no valid execution where load1 has a value (bottom).
  // if row_var was open, load1 could be anything (top).
  set_key<scope>("load1"_h, addresses::bottom());
  results.back().state.apply<state::idx<memory>>([&](memory* m) { m->set_to_top(); });

  add_node(alloc{"alloc2"_h, "unknown"_h});  // has no effect because state is invalid already.
  add_node(ref{"ref2"_h, "alloc1"_h});       // has no effect because state is invalid already.

  const auto& [program, state] = GENERATE(from_range(results));
  INFO(std::format("\nprogram:\n{:lines}", resolve_handles(program, handle_pool.handles())));
  INFO(std::format("\nexpected:\n{:lines}", with_handles{state, handle_pool.handles()}));

  auto observed = analyse(program);

  INFO(std::format("\nobserved:\n{:lines}\n\n", with_handles{observed, handle_pool.handles()}));
  INFO(std::format("\nhandles:\n{}", handle_pool.handles()));
  bool analysis_inferred_correct_state = state == observed;
  REQUIRE(analysis_inferred_correct_state);
}

}  // namespace stanly
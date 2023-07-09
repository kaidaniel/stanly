
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <format>
#include <map>
#include <vector>

#include "analyse.h"
#include "domain.h"
#include "string-index.h"
#include "syntax.h"

namespace stanly {

using namespace domains;
using namespace syntax;
using enum RowVarEls;

struct result {
  std::vector<ast_node> nodes{};
  state state{};
};
std::vector<result> results{result{{}, {}}};
void add_node(ast_node&& n) {
  results.push_back(results.back());
  results.back().nodes.push_back(n);
}
template <class Target>
void set_key(auto&&... args) {
  results.back().state.set_key<Target>(args...);
}

TEST_CASE("analyse a basic block node-by-node", "[ast_node][analyse]") {
  add_node(alloc{"alloc1"_h, "unknown"_h});
  set_key<scope>("alloc1"_h, addresses{"alloc1"_h});
  set_key<memory>("alloc1"_h,
                  object{{type{"unknown"_h}, data{record{{row_var{Closed}, defined{}, used{}}}}}});

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
              data{record{
                  {row_var{Closed}, defined{{{"field1_val"_h, addresses{"123"_h}}}}, used{}}}}}});

  add_node(ref{"ref1"_h, "alloc1"_h});
  set_key<scope>("ref1"_h, addresses{"alloc1"_h});

  add_node(load{"load1"_h, "alloc1"_h, "field1"_h});
  set_key<scope>("load1"_h, addresses{"123"_h});
  set_key<memory>(
      "alloc1"_h,
      object{{type{"unknown"_h},
              data{record{{row_var{Closed}, defined{{{"field1_val"_h, addresses{"123"_h}}}},
                           used{"field1_val"_h}}}}}});

  add_node(lit{"field2"_h, "str"_h, "field2_val"_h});
  set_key<scope>("field2"_h, addresses{"field2_val"_h});
  set_key<memory>("field2_val"_h, object{{type{"str"_h}, data{constant{"field2_val"_h}}}});

  add_node(alloc{"alloc1"_h, "dict"_h});
  set_key<scope>("alloc1"_h, addresses{"alloc1"_h});
  set_key<memory>("alloc1"_h,
                  object{{type{"dict"_h}, data{record{{row_var{Closed}, defined{}, used{}}}}}});

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
  INFO(std::format("\nprogram:\n{:lines}", resolve_handles(program)));
  INFO(std::format("\nexpected:\n{:lines}", with_handles{state}));

  auto observed = analyse(program);

  INFO(std::format("\nobserved:\n{:lines}\n\n", with_handles{observed}));
  INFO(std::format("\nhandles:\n{}", global_string_index.handles()));
  bool analysis_inferred_correct_state = state == observed;
  REQUIRE(analysis_inferred_correct_state);
}

}  // namespace stanly
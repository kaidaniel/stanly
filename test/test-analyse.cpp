
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <format>
#include <vector>

#include "analyse.h"
#include "catch2/catch_message.hpp"
#include "catch2/generators/catch_generators.hpp"
#include "domain.h"
#include "handle.h"
#include "string-index.h"
#include "syntax.h"

namespace stanly {

using enum RowVarEls;

struct result {
  std::vector<node> nodes{};
  state state{};
};

template <class Target>
struct set_key_struct {
  std::vector<result>& results;
  void
  operator()(const handle& key, const auto& val) {
    results.back().state([&](Target* t) { t->set(key, val); });
  }
};

std::vector<result> results{result{{}, {}}};  // NOLINT

TEST_CASE("analyse a basic block node-by-node", "[node][analyse]") {
  auto add_node = [&](node n) {
    results.push_back(results.back());
    results.back().nodes.push_back(n);
  };

  auto set_scope = set_key_struct<scope>(results);
  auto set_memory = set_key_struct<memory>(results);
  add_node(alloc{"alloc1"_h, "unknown"_h});
  set_scope("alloc1"_h, addresses{"alloc1"_h});
  set_memory(
      "alloc1"_h,
      object{{type{"unknown"_h}, record{{row_var{Closed}, defined{}, used{}}}, constant{}}});

  add_node(lit{"lit1"_h, "int"_h, "123"_h});
  set_scope("lit1"_h, addresses{"123"_h});
  set_memory("123"_h, object{{type{"int"_h}, record{}, constant{"123"_h}}});

  add_node(lit{"field1"_h, "str"_h, "field1_val"_h});
  set_scope("field1"_h, addresses{"field1_val"_h});
  set_memory("field1_val"_h, object{{type{"str"_h}, record{}, constant{"field1_val"_h}}});

  add_node(update{"alloc1"_h, "field1"_h, "lit1"_h});
  set_memory(
      "alloc1"_h,
      object{
          {type{"unknown"_h},
           record{{row_var{Closed}, defined{{{"field1_val"_h, addresses{"123"_h}}}}, used{}}},
           constant{}}});

  add_node(ref{"ref1"_h, "alloc1"_h});
  set_scope("ref1"_h, addresses{"alloc1"_h});

  add_node(read{"read1"_h, "alloc1"_h, "field1"_h});
  set_scope("read1"_h, addresses{"123"_h});
  set_memory(
      "alloc1"_h, object{
                      {type{"unknown"_h},
                       record{
                           {row_var{Closed}, defined{{{"field1_val"_h, addresses{"123"_h}}}},
                            used{"field1_val"_h}}},
                       constant{}}});

  add_node(lit{"field2"_h, "str"_h, "field2_val"_h});
  set_scope("field2"_h, addresses{"field2_val"_h});
  set_memory("field2_val"_h, object{{type{"str"_h}, record{}, constant{"field2_val"_h}}});

  add_node(alloc{"alloc1"_h, "dict"_h});
  set_scope("alloc1"_h, addresses{"alloc1"_h});
  results.back().state([&](memory* m) {
    m->set(
        "alloc1"_h,
        object{{type{"dict"_h}, record{{row_var{Closed}, defined{}, used{}}}, constant{}}});
  });

  add_node(ref{"ref1"_h, "alloc1"_h});
  set_scope("ref1"_h, addresses{"alloc1"_h});

  add_node(read{"read1"_h, "alloc1"_h, "field2"_h});
  // row var is closed: no valid execution where read1 has a value (bottom).
  // if row_var was open, read1 could be anything (top).
  set_scope("read1"_h, addresses::bottom());
  results.back().state([&](memory* m) { m->set_to_top(); });

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
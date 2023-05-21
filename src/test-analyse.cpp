#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <string_view>
#include <vector>

// #include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "HashedAbstractPartition.h"
#include "domains.h"
#include "syntax.h"

namespace stanly {

TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  struct programs : nodes {
    std::vector<firstorder> operator()() {
      return {alloc{"al", "dict"}, lit{"lt", "123"},       update{"al", "f", "lt"},
              ref{"rf", "al"},     load{"ld0", "al", "f"}, load{"ld1", "al", "g"},
              alloc{"al", "top"},  ref{"rf", "al"}};
    }
  };

  struct collected_states : public domains, public nodes {
    std::vector<std::pair<std::vector<nodes::firstorder>, domain>> operator()() {
      std::vector<std::pair<std::vector<firstorder>, domain>> result{};
      std::vector<firstorder> nodes{};
      auto add_node = [&result, &nodes](firstorder&& n, scope&& s, memory&& m) {
        nodes.push_back(n);
        s.join_with(result.back().second.get<0>());
        m.join_with(result.back().second.get<1>());
        result.push_back({nodes, state{{s, m}}});
      };
      // clang-format off
      add_node({}, 
        bottom, 
        bottom);

      add_node(alloc{"al", "top"},
        scope{{{"al", addresses{"al"}}}},
        memory{{"al.0", object{{type{"dict"}, value::bottom()}}}});

      add_node(lit{"lt", "123"},
        scope{{"lt", addresses{"lt"}}},
        memory{{"lt", object{{type{"integer"}, constant{"123"}}}}});

      add_node(update("al", "f", "lt"),
        bottom,
        memory{{"d", object{{type{"dict"}, record{{defined{{{"f", addresses{"lt"}}}}, bottom}}}}}});

      add_node(ref("rf", "al"),
        scope{{"rf", addresses{"al"}}},
        bottom);

      add_node(load{"ld", "al", "f"},
        scope{{"ld", addresses{"lt"}}},
        memory{{"al", object{{bottom, record{{bottom, used{"f"}}}}}}});

      add_node(load{"ld", "al", "g"},
        scope{{"ld", top}},
        memory{{"al", object{{bottom, record{{bottom, used{"g"}}}}}}});
      // clang-format on

      // analyse([..., ref(rf al)])

      // auto update = nodes::update{.tgt="d", .field="f", .src="l"};
      // mem.update("l",
      // [f=update.field](object* d){ d->apply<1>(
      //   [f](value* vl){vl->apply<record>(
      //     [f](record* rd){
      //       rd->apply<0>([f](bindings* bs){
      //         bs->update(f, [f](addresses* as){
      //           as->add(f);});});
      //       rd->apply<1>([f](fields* fs){
      //         fs->add(f);
      //       });
      // });});});
      return result;
    }
  };

  // struct x : domains {
  //   std::vector<domain> operator()() {
  //     bindings b = bindings{{"x", addresses{"x"}}};
  //     fields f1 = fields{"a"};
  //     fields f2 = fields{};
  //     fields f3 = {};
  //     record r1 = record{{b, f1}};
  //     record r2 = record{{bindings{{"x", addresses{"x"}}}, fields{"a"}}};
  //     object o1 = object{{type{"dict"}, r2}};
  //     object o2 = object{{type{"dict"}, record{{bindings{{"x", addresses{"x"}}}, fields{}}}}};
  //     memory m1 = memory{{"x", o2}};
  //     memory m2 = memory{
  //         {"x", object{{type{"dict"}, record{{bindings{{"x", addresses{"x"}}}, fields{}}}}}}};
  //     scope s1 = scope{{"x", addresses{"x"}}};
  //     domain d1 = domain{{s1, m2}};
  //     domain d2 = domain{
  //         {scope{{"x", addresses{"x"}}},
  //          memory{{"x",
  //                  object{{type{"dict"}, record{{bindings{{"x", addresses{"x"}}},
  //                  fields{}}}}}}}}};
  //     return {};
  //   }
  // };
  CHECK(1 + 5 == 1 + 5);
  // CHECK_THAT(bindings{}(), Catch::Matchers::RangeEquals(vw::transform(programs{}(), analyse)));
}
}  // namespace stanly

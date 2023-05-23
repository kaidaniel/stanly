
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <ranges>
#include <vector>

#include "analyse.h"
#include "domain.h"
#include "syntax.h"

namespace stanly {
TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  class collected_states : public domains, public nodes {
    struct result {
      std::vector<firstorder> nodes{};
      state state{};
    };
    std::vector<result> results{result{{}, {}}};
    void add_node(firstorder&& n) {
      results.push_back(results.back());
      results.back().nodes.push_back(n);
    }

   public:
    state& res() { return results.back().state; }
    std::vector<result> operator()() && {
      add_node(alloc{"al", "top"});
      res().set_var("al", addresses{"al"});
      res().set_mem("al", object{{top, data::top()}});

      add_node(lit{"lt", "123"});
      res().set_var("lt", addresses{"lt"});
      res().set_mem("lt", object{{type{"integer"}, data{constant{"123"}}}});

      add_node(update{"al", "f", "lt"});
      res().set_mem("al",
                    object{{top, data{record{{top, defined{{{"f", addresses{"lt"}}}}, bot}}}}});

      add_node(ref{"rf", "al"});
      res().set_var("rf", addresses{"al"});

      add_node(load{"ld", "al", "f"});
      res().set_var("ld", addresses{"lt"});
      res().set_mem(
          "al", object{{top, data{record{{top, defined{{{"f", addresses{"lt"}}}}, used{"f"}}}}}});

      add_node(load{"ld", "al", "g"});
      res().set_var("ld", top);
      res().set_mem(
          "al",
          object{{top, data{record{{top, defined{{{"f", addresses{"lt"}}}}, used{"f", "g"}}}}}});

      add_node(alloc{"al", "dict"});
      res().set_var("al", addresses{"al"});

      add_node(ref{"rf", "al"});
      res().set_var("rf", addresses{"al"});

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
      return results;
    }
  };
  auto [program, state] = GENERATE(from_range(collected_states{}()));
  CHECK(analyse(program) == state);
}
}  // namespace stanly
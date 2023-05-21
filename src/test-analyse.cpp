#include <catch2/catch_test_macros.hpp>
#include <ranges>
#include <string_view>
#include <vector>

// #include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "HashedAbstractPartition.h"
#include "catch2/matchers/catch_matchers_range_equals.hpp"
#include "domains.h"
#include "syntax.h"

namespace stanly {

TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  struct collected_states : public domains, public nodes {
    struct result {
      std::vector<firstorder> nodes{};
      domain state{};
    };
    std::vector<state> states{};
    std::vector<std::vector<firstorder>> programs{};
    void add_node(firstorder&& n) {
      programs.push_back(programs.back());
      programs.back().push_back(std::move(n));
      states.push_back(states.back());
    }

    state& res() { return states.back(); }
    std::pair<std::vector<std::vector<firstorder>>, std::vector<state>> operator()() && {
      add_node({});

      add_node(alloc{"al", "top"});
      res().set_var("al", addresses{"al"});
      res().set_mem("0.al", object::top());

      add_node(lit{"lt", "123"});
      res().set_var("lt", addresses{"lt"});
      res().set_mem("lt", object{constant{"123"}, type{"integer"}});

      add_node(update{"al", "f", "lt"});
      res().set_mem("al", defined{{{"f", addresses{"lt"}}}});

      add_node(ref{"rf", "al"});
      res().set_var("rf", addresses{"al"});

      add_node(load{"ld", "al", "f"});
      res().set_var("ld", addresses{"lt"});
      res().join_mem(memory{{"al", object{used{"f"}}}});

      add_node(load{"ld", "al", "g"});
      res().set_var("ld", top);
      res().join_mem(memory{{"al", object{used{"g"}}}});

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
      return {programs, states};
    }
  };
  auto analyse = [](std::vector<nodes::firstorder> n) -> domains::state { return {}; };
  auto [programs, states] = collected_states{}();
  //CHECK_THAT(states, Catch::Matchers::RangeEquals(vw::transform(programs, analyse)));
}
}  // namespace stanly

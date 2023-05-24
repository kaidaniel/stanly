
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <ranges>
#include <vector>

#include "analyse.h"
#include "domain.h"
#include "syntax.h"

namespace stanly {
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
  template <class Target, class... Args>
  void set_key(Args&&... args) {
    results.back().state.set_key<Target>(std::forward<Args>(args)...);
  }

 public:
  state& res() { return results.back().state; }
  std::vector<result> operator()() && {
    add_node(alloc{"al", "top"});
    set_key<scope>("al", addresses{"al"});
    set_key<memory>("al", object{{top, data::top()}});

    add_node(lit{"lt", "123"});
    set_key<scope>("lt", addresses{"lt"});
    set_key<memory>("lt", object{{type{"integer"}, data{constant{"123"}}}});

    add_node(update{"al", "f", "lt"});
    set_key<memory>("al",
                    object{{top, data{record{{top, defined{{{"f", addresses{"lt"}}}}, bot}}}}});

    add_node(ref{"rf", "al"});
    set_key<scope>("rf", addresses{"al"});

    add_node(load{"ld", "al", "f"});
    set_key<scope>("ld", addresses{"lt"});
    set_key<memory>(
        "al", object{{top, data{record{{top, defined{{{"f", addresses{"lt"}}}}, used{"f"}}}}}});

    add_node(load{"ld", "al", "g"});
    set_key<scope>("ld", top);
    set_key<memory>(
        "al",
        object{{top, data{record{{top, defined{{{"f", addresses{"lt"}}}}, used{"f", "g"}}}}}});

    add_node(alloc{"al", "dict"});
    set_key<scope>("al", addresses{"al"});

    add_node(ref{"rf", "al"});
    set_key<scope>("rf", addresses{"al"});

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
TEST_CASE("analyse firstorder programs", "[firstorder][analyse]") {
  auto [program, state] = GENERATE(from_range(collected_states{}()));
  CHECK(analyse(program) == state);
}
}  // namespace stanly
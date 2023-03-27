#include <sys/types.h>

#include <forward_list>
#include <iostream>
#include <set>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <variant>
#include <vector>

#include "range/v3/view.hpp"
#include "syntax.h"

namespace stanly {

using ranges::views::transform;
using std::forward_list;
using std::is_same_v;
using std::make_from_tuple;
using std::set;
using std::string;
using std::string_view;
using std::unordered_map;
using std::vector;
using std::visit;

class SourceTextIndex {
  // all_text_references_: set because long strings slow to hash (?redex)
  // program_source_texts_: adding to a forward list won't invalidate
  // references. insert_text_reference: bounds checked, <=
  // std::numeric_limits<Idx>::max() idx_to_text_reference: Not bounds checked
  set<string_view> all_text_references_{};
  vector<string_view> idx_to_text_reference_{};
  forward_list<string> source_texts_{};

 public:
  const idx &insert_text_reference(string_view);
  const string_view &idx_to_text_reference(idx);
  string_view add_source_text(string_view);
};
struct SourceTextLocation {
  int program;
  int start;
  int end;
  int col;
  int row;
};

template <class A, class B>
struct rebind;

template <template <class> class T, class x, class y>
struct rebind<T<x>, y> {
  using type = T<y>;
};

template <class A, class B>
using rebind_t = typename rebind<A, B>::type;

template <template <class> class S>
  requires syntax<S<idx>> && syntax<S<string_view>>
class GraphT {
  using node = typename S<idx>::node;
  using repr = typename S<idx>::repr;
  unordered_map<node, SourceTextLocation> node_to_source_text_location_;
  vector<node> nodes_{};
  unordered_map<repr, std::vector<repr>> record_literals_{};
  SourceTextIndex source_text_index_{};

 public:
  [[nodiscard]] decltype(auto) nodes() {
    using node_view = typename S<string_view>::node;
    auto idx_to_str = [&](idx idx) { return source_text_index_.idx_to_text_reference(idx); };
    auto node_to_str_node = [&]<class N>(N &&n) {
      return make_from_tuple<rebind_t<N, string_view>>(map_tpl(idx_to_str, to_tpl(n)));
    };
    return transform(nodes_, [&](node n) -> node_view { return {visit(node_to_str_node, n)}; });
  };
  GraphT(string_view program);
  GraphT(const GraphT &) = delete;
  GraphT(GraphT &&) = delete;
  GraphT operator=(GraphT &&) = delete;
  GraphT operator=(const GraphT &) = delete;
  ~GraphT() = default;
};

// Graph (*(make_parser)(std::string_view language))(std::string_view) {
//   if (language == "firstorder") {
//     ;
//   }
// }
}  // namespace stanly

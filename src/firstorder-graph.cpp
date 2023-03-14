#include "firstorder-lang.h"
#include "metaprogramming.h"
#include "range/v3/view.hpp"
#include "stanly-api.h"
#include <forward_list>
#include <iostream>
#include <set>
#include <string>
#include <string_view>
#include <sys/types.h>
#include <unordered_map>
#include <variant>
#include <vector>

namespace stanly {

using ranges::views::transform;
using stanly::metaprogramming::struct_to_tpl;
using std::bind_front;
using std::decay;
using std::forward_list;
using std::is_same_v;
using std::set;
using std::string_view;
using std::unordered_map;
using std::vector;
using std::visit;

template <class Text> using lang = first_order<Text>;

class ProgramSourceTextIndex {
  // all_text_references_: set because long strings slow to hash (?redex)
  // program_source_texts_: adding to a forward list won't invalidate
  // references. insert_text_reference: bounds checked, <=
  // std::numeric_limits<Idx>::max() idx_to_text_reference: Not bounds checked
  std::set<std::string_view> all_text_references_{};
  std::vector<std::string_view> idx_to_text_reference_{};
  std::forward_list<std::string> program_source_texts_{};
public:
  const idx &insert_text_reference(std::string_view);
  const std::string_view &idx_to_text_reference(idx);
  std::string_view add_program_source(std::string_view);
};
struct SourceTextLocation {
  int program;
  int start;
  int end;
  int col;
  int row;
};

template <class Lang> class GraphT {
  std::unordered_map<syntax_packed, SourceTextLocation>
      syntax_node_to_source_text_offsets_;
  std::vector<syntax_packed> syntax_nodes_{};
  std::unordered_map<idx, packed::record> record_literals_{};
  ProgramSourceTextIndex program_source_text_index_{};
public:
  [[nodiscard]] decltype(auto) nodes_view();
  GraphT(std::string_view program);
  GraphT(const GraphT &) = delete;
  GraphT(GraphT &&) = delete;
  GraphT operator=(GraphT &&) = delete;
  GraphT operator=(const GraphT &) = delete;
  ~GraphT() = default;
};

template <class Lang> [[nodiscard]] decltype(auto) GraphT<Lang>::nodes_view() {
  auto get = bind_front(
      &ProgramSourceTextIndex::idx_to_text_reference,
      &program_source_text_index_);
  return transform(
      syntax_nodes_, [&](syntax_packed variant) -> syntax_printable {
        return visit(
            [&]<class T>(T &&n) -> syntax_printable {
              using t = lookup<T, packed_typelist, printable_typelist>;
              return tpl_to_struct<t>(transform_tpl(get, struct_to_tpl(n)));
            },
            variant);
      });
};

template <class Lang> GraphT<Lang>::GraphT(std::string_view program) {
  using iterator::inpt_range;
  program = program_source_text_index_.add_program_source(program);
  for (auto nodes = parse_firstorder(program); print node : nodes) {}
}

// Graph (*(make_parser)(std::string_view language))(std::string_view) {
//   if (language == "firstorder") {
//     ;
//   }
// }
} // namespace stanly
#include "language.h"
#include "metaprogramming.h"
#include "range/v3/view.hpp"
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

template <class F, class Container, language L, class Repr, class ReturnRepr>
auto transmute_syntax_container(F &&f, Container &&c) {
  return ranges::views::transform(
      std::forward<Container>(c), [&f](syntax<L, Repr> variant) {
        return std::visit(
            [&f, &variant]<class T>(T &&node) {
              return metaprogramming::transmute<
                  T, L::template typelist<Repr>,
                  L::template typelist<ReturnRepr>>(f, std::forward<T>(node));
            },
            std::forward<decltype(variant)>(variant));
      });
}

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
  ProgramSourceTextIndex(std::string_view program) {
    add_program_source(program);
  }
};
struct SourceTextLocation {
  int program;
  int start;
  int end;
  int col;
  int row;
};

template <language L> class GraphT {
  std::unordered_map<packed<L>, SourceTextLocation>
      syntax_node_to_source_text_offsets_;
  std::vector<packed<L>> syntax_nodes_;
  std::unordered_map<idx, record<packed<L>>> record_literals_{};
  ProgramSourceTextIndex program_source_text_index_;
public:
  [[nodiscard]] decltype(auto) nodes_view() {
    return transmute_syntax_container(
        [&](idx idx) {
          return program_source_text_index_.idx_to_text_reference(idx);
        },
        syntax_nodes_);
  }
  GraphT(std::string_view program)
      : program_source_text_index_{program},
        syntax_nodes_{ranges::views::transform(
            parse<L>(program),
            transmute_syntax<L, std::string_view, idx>(
                [&](std::string_view sv) {
                  return program_source_text_index_.insert_text_reference(sv);
                }))} {};
  GraphT(const GraphT &) = delete;
  GraphT(GraphT &&) = delete;
  GraphT operator=(GraphT &&) = delete;
  GraphT operator=(const GraphT &) = delete;
  ~GraphT() = default;
};

} // namespace stanly
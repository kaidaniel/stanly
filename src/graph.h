#include <sys/types.h>

#include <forward_list>
#include <iostream>
#include <ranges>
#include <set>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include "syntax.h"

namespace stanly {

template <template <class> class VariantFor, class x, class y>
auto map_to_same_name(const std::vector<VariantFor<x>> &vec, const std::function<y(x const &)> f) {
  auto inj_x_to_inj_y = [&]<class InjX>(InjX &&inj_x) {
    using inj_y = search_same_name_t<InjX, VariantFor<y>>;
    return std::apply([&](auto &&...i) -> inj_y { return {f(i)...}; }, to_tpl(inj_x));
  };
  using std::ranges::views::transform;
  return vec | transform(std::bind_front(std::visit, inj_x_to_inj_y));
}

//  auto variant_to_variant = [&](VariantFor<x> &&variant) -> VariantFor<y> {
//    return std::visit(inj_x_to_inj_y, variant);
//  };

/*
class StringIndex {
  // all_text_references_: set because long strings slow to hash (?redex)
  // program_source_texts_: adding to a forward list won't invalidate references.
  // insert_text_reference: bounds checked, <= std::numeric_limits<Idx>::max()
  // idx_to_text_reference: Not bounds checked
  std::set<std::string_view> all_text_references_{};
  std::vector<std::string_view> idx_to_text_reference_{};
  std::forward_list<std::string> strings_{};

 public:
  const idx &insert(std::string_view);
  const std::string_view &get_sv(idx);
  std::string_view add_string(std::string_view);
  StringIndex(std::string_view program);
};
struct SourceTextLocation {
  int program;
  int start;
  int end;
  int col;
  int row;
};

template <template <class> class SyntaxT>
  requires packed_syntax<typename SyntaxT<idx>::node> &&
           syntax<typename SyntaxT<std::string_view>::node>
class Graph {
  using packed_stx = SyntaxT<idx>::node;
  using unpacked_stx = SyntaxT<std::string_view>::node;
  std::unordered_map<packed_stx, SourceTextLocation> syntax_node_to_source_text_offsets_;
  std::vector<packed_stx> syntax_nodes_;
  std::unordered_map<idx, std::vector<packed_stx>> record_literals_{};
  StringIndex string_index_;

 public:
  auto nodes_view() {
    return std::ranges::views::transform(
        [this](packed_stx &&packed_stx) -> unpacked_stx {
          return std::visit(
              [this]<class PackedNode>(PackedNode &&packed_node) {
                using unpacked_node = type_with_same_name_in_t<PackedNode, unpacked_stx>;
                return std::apply(
                    [this](auto... i) -> unpacked_node { return {string_index_.get_sv(i)...}; },
                    to_tpl(packed_node));
              },
              packed_stx);
        },
        syntax_nodes_);
  }
  Graph(std::string_view);
  //  Graph(std::string_view program)
  //      : program_source_text_index_{program},
  //        syntax_nodes_{std::ranges::views::transform(
  //            parse<Syntax>(program),
  //            transmute_syntax<L, std::string_view, idx>([&](std::string_view sv) {
  //              return program_source_text_index_.insert_text_reference(sv);
  //            }))} {};
  Graph(const Graph &) = delete;
  Graph(Graph &&) = delete;
  Graph operator=(Graph &&) = delete;
  Graph operator=(const Graph &) = delete;
  ~Graph() = default;
};
*/
}  // namespace stanly

#pragma once

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

#include "stanly-utils.h"
#include "syntax.h"

namespace stanly {

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

template <packed_syntax Syntax, syntax UnpackedSyntax>
class Graph {
  std::unordered_map<Syntax, SourceTextLocation> syntax_node_to_source_text_offsets_;
  std::vector<Syntax> syntax_nodes_;
  std::unordered_map<idx, std::vector<Syntax>> record_literals_{};
  StringIndex string_index_;

 public:
  auto view_syntax() {
    auto get = [this](idx i) { return string_index_.get_sv(i); };
    auto unpack = map_to_same_name<Syntax, UnpackedSyntax>(get);
    return syntax_nodes_ | std::ranges::views::transform(unpack);
  }
  Graph(std::vector<Syntax> syntax_nodes_);
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

}  // namespace stanly

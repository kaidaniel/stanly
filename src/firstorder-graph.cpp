#include "firstorder-lang.h"
#include "metaprogramming.h"
#include "range/v3/view.hpp"
#include "stanly-api.h"
#include <variant>
#ifndef NDEBUG
#  include <boost/stacktrace.hpp>

#endif
#include <forward_list>
#include <iostream>
#include <set>
#include <string>
#include <string_view>
#include <sys/types.h>
#include <unordered_map>
#include <vector>

namespace stanly {

using stanly::metaprogramming::rebind_t;

struct Idx {
  uint16_t idx_;
};

class ProgramSourceTextIndex {
  // all_text_references_: set because long strings slow to hash (?redex)
  // program_source_texts_: adding to a forward list won't invalidate
  // references. insert_text_reference: bounds checked, <=
  // std::numeric_limits<Idx>::max() idx_to_text_reference: Not bounds checked
  std::set<std::string_view> all_text_references_;
  std::vector<std::string_view> idx_to_text_reference_{};
  std::forward_list<std::string> program_source_texts_;
public:
  const Idx &insert_text_reference(std::string_view);
  const std::string_view &idx_to_text_reference(Idx);
  std::string_view add_program_source(std::string_view);
};
struct SourceTextLocation {
  int program;
  int start;
  int end;
  int col;
  int row;
};
struct Subscript {
  Idx object{};
  Idx field{};
};
enum class kFirstOrderSyntax : char {
  kSetField,
  kLoadField,
  kLoadText,
  kLoadRecord,
  kLoadVar,
  kLoadTop,
};
struct BytePackedSyntax {
  union {
    Subscript subscript{};
    Idx text_idx;
    Idx record_idx;
    Idx load_var_rhs;
  };
  Idx var_idx{};
  kFirstOrderSyntax syntax_tag{};
};
class FirstOrderGraph {
  std::unordered_map<BytePackedSyntax, SourceTextLocation>
      syntax_node_to_source_text_offsets_{};
  std::vector<BytePackedSyntax> syntax_nodes_{};
  std::unordered_map<Idx, RecordLiteral> record_literals_{};
  ProgramSourceTextIndex program_source_text_index_{};

  void insert(Syntax);
public:
  [[nodiscard]] decltype(auto) nodes_view();
  FirstOrderGraph(std::string_view program);
  FirstOrderGraph(const FirstOrderGraph &) = delete;
  FirstOrderGraph(FirstOrderGraph &&) = delete;
  FirstOrderGraph operator=(FirstOrderGraph &&) = delete;
  FirstOrderGraph operator=(const FirstOrderGraph &) = delete;
  ~FirstOrderGraph() = default;
};

[[nodiscard]] decltype(auto) FirstOrderGraph::nodes_view() {
  using enum kFirstOrderSyntax;
  auto get = std::bind_front(
      &ProgramSourceTextIndex::idx_to_text_reference,
      &program_source_text_index_);
  return ::ranges::views::transform(
      syntax_nodes_, [&](BytePackedSyntax n) -> Syntax {
        auto object = get(n.subscript.object);
        auto field = get(n.subscript.field);
        auto var = get(n.var_idx);
        auto text_literal = get(n.text_idx);
        auto load_var_rhs = get(n.load_var_rhs);
        auto &record_literal = record_literals_[n.record_idx];
        switch (n.syntax_tag) {
        case kSetField: return SetField{var, object, field};
        case kLoadField: return LoadField{var, object, field};
        case kLoadText: return LoadText{var, text_literal};
        case kLoadRecord: return LoadRecord{var, record_literal};
        case kLoadVar: return LoadVar{var, load_var_rhs};
        case kLoadTop: return LoadTop{var, text_literal};
        };
      });
}

FirstOrderGraph::FirstOrderGraph(std::string_view program) {
  program = program_source_text_index_.add_program_source(program);
  auto syntax_nodes = parse_firstorder(program);
  for (auto node : *syntax_nodes) { insert(std::move(node)); }
}

// Graph (*(make_parser)(std::string_view language))(std::string_view) {
//   if (language == "firstorder") {
//     ;
//   }
// }
} // namespace stanly
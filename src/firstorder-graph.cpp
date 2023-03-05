#include "firstorder-lang.h"
#include "metaprogramming.h"
#include "range/v3/view.hpp"
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

class Idx {
  uint16_t idx_;
  Idx(uint16_t idx)
      : idx_(idx) {} // private constructor to make idx_to_text_reference safer
public:
  Idx() = delete;
  friend class ProgramSourceTextIndex;
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
  Idx insert_text_reference(std::string_view);
  std::string_view idx_to_text_reference(Idx);
  void add_program_source(std::string_view);
};
struct SourceTextLocation {
  int program;
  int start;
  int end;
  int col;
  int row;
};
struct Subscript {
  Idx subscripted;
  Idx subscripting;
};
enum class kFirstOrderSyntax : char {
  kSetField,
  kLoadField,
  kLoadText,
  kLoadRecord,
  kLoadVar
};
struct BytePackedSyntax {
  union {
    Subscript subscript;
    Idx text_idx;
    Idx record_idx;
    Idx load_var_rhs;
  };
  Idx var_idx;
  kFirstOrderSyntax syntax_tag;
};
class FirstOrderGraph {
  std::unordered_map<BytePackedSyntax, SourceTextLocation>
      syntax_node_to_source_text_offsets_;
  std::vector<BytePackedSyntax> syntax_nodes_;
  std::unordered_map<Idx, std::vector<std::string_view>> record_literals_;
  ProgramSourceTextIndex program_source_text_index_;
public:
  [[nodiscard]] decltype(auto) nodes_view();
  FirstOrderGraph(std::function<std::string(void)>);
  FirstOrderGraph(const FirstOrderGraph &) = delete;
  FirstOrderGraph(FirstOrderGraph &&) = delete;
  FirstOrderGraph operator=(FirstOrderGraph &&) = delete;
  FirstOrderGraph operator=(const FirstOrderGraph &) = delete;
  ~FirstOrderGraph() = default;
};

[[nodiscard]] decltype(auto) FirstOrderGraph::nodes_view() {
  using enum kFirstOrderSyntax;
  auto get = [&](Idx var_idx) {
    return program_source_text_index_.idx_to_text_reference(var_idx);
  };
  return ::ranges::views::transform(
      syntax_nodes_,
      [&](BytePackedSyntax n)
          -> metaprogramming::rebind<std::variant, FirstOderSyntaxNodes>::type {
        switch (n.syntax_tag) {
        case kSetField:
          return SetField{
              get(n.var_idx), get(n.subscript.subscripted),
              get(n.subscript.subscripting)};
        case kLoadField:
          return LoadField{
              get(n.var_idx), get(n.subscript.subscripted),
              get(n.subscript.subscripting)};
        case kLoadText: return LoadText{get(n.var_idx), get(n.text_idx)};
        case kLoadRecord:
          return LoadRecord{get(n.var_idx), record_literals_[n.record_idx]};
        case kLoadVar: return LoadVar{get(n.var_idx), get(n.load_var_rhs)};
        };
      });
}
} // namespace stanly
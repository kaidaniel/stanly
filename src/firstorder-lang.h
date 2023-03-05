#pragma once

#include "ConstantAbstractDomain.h"
#include "DirectProductAbstractDomain.h"
#include "HashedAbstractEnvironment.h"
#include "HashedSetAbstractDomain.h"
#include "stanly-api.h"
#include "metaprogramming.h"
#include <sys/types.h>
#include <vector>
#include <string_view>
#include <unordered_map>
#include <set>
#include <forward_list>



namespace stanly {

using Var = std::string_view;
using TextLiteral = std::string_view;
using RecordLiteral = std::vector<TextLiteral>;
/// Abstraction of a const-propagated literal.
using Text = sparta::ConstantAbstractDomain<TextLiteral>;
/// Abstraction of a dynamic record as a set of field names.
using Record = sparta::HashedSetAbstractDomain<TextLiteral>;
/// Abstraction of a set of values in memory (elements or objects).
struct Value : public sparta::DirectProductAbstractDomain<Value, Text, Record> {
  using Product = sparta::DirectProductAbstractDomain<Value, Text, Record>;
  using Product::DirectProductAbstractDomain;
};
/// Abstraction of the program state (Var -> Value).
using Bindings = sparta::HashedAbstractEnvironment<Var, Value>;
// clang-format off
/// `target` [ `field` ] = `rhs`, e.g. `x[y]=z`
struct SetField { Var rhs; Var target; Var field; };
/// `lhs` = `source` [ `subscript` ], e.g. `x=y[z]`
struct LoadField { Var lhs; Var source; Var field; };
/// `lhs` = `text_literal`, e.g. `x="abc"` or `x=1`
struct LoadText { Var lhs; TextLiteral text_literal; };
/// `lhs` = `record`, e.g. `x={"a": 1, "b": 2}`
struct LoadRecord { Var lhs; RecordLiteral record_literal; };
/// `lhs` = `rhs`
struct LoadVar { Var lhs; Var rhs; };
// clang-format on

using FirstOderSyntaxNodes = metaprogramming::TypeList<SetField, LoadField, LoadText, LoadRecord, LoadVar>;

class Idx {
  uint16_t idx_;
  Idx(uint16_t idx) : idx_(idx) {}  // private constructor to make idx_to_text_reference safer
  public:
  Idx() = delete;
  friend class ProgramSourceTextIndex;
};

class ProgramSourceTextIndex {
  // set (not unordered set) because comparing long strings is faster than hashing them (?)
  std::set<std::string_view> all_text_references_;
  std::vector<std::string_view> idx_to_text_reference_{};
  // adding elements to a forward list won't invalidate references to elements.
  // each element is the source from one file.
  std::forward_list<std::string> program_source_texts_;
  public:
  // Bounds checked: insert at most: std::numeric_limits<Idx>::max() (size of the index).
  Idx insert_text_reference(std::string_view);
  // Not bounds checked
  std::string_view idx_to_text_reference(Idx);
  void add_program_source(std::string_view);
};


using Kind = sparta::AbstractValueKind;
class FirstOrderGraph {
  struct SourceTextLocation{int program; int start; int end; int col; int row; };
  struct Subscript {Idx subscripted; Idx subscripting;};
  enum class kFirstOrderSyntax : char { kSetField, kLoadField, kLoadText, kLoadRecord, kLoadVar};
  struct BytePackedSyntax {
    union{
      Subscript subscript;
      Idx text_idx;
      Idx record_idx;
      Idx load_var_rhs;
    };
    Idx var_idx;
    kFirstOrderSyntax syntax_tag;
  };
  
  std::unordered_map<BytePackedSyntax, SourceTextLocation> syntax_node_to_source_text_offsets_;
  std::vector<BytePackedSyntax> syntax_nodes_;
  std::unordered_map<Idx, RecordLiteral> record_literals_;
  ProgramSourceTextIndex program_source_text_index_;

public:
  [[nodiscard]] decltype(auto) nodes_view();
  FirstOrderGraph(std::string_view program);
  FirstOrderGraph(const FirstOrderGraph&) = delete;
  FirstOrderGraph(FirstOrderGraph&&) = delete;
  FirstOrderGraph operator=(FirstOrderGraph&&) = delete;
  FirstOrderGraph operator=(const FirstOrderGraph&) = delete;
  ~FirstOrderGraph() = default;
};
class FirstOrderAnalysis;
Analysis analyse(const FirstOrderGraph &);
} // namespace stanly
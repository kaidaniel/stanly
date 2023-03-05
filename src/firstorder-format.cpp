#include "firstorder-lang.h"
#include <cstddef>
#include <fmt/format.h>
#include <functional>
#include <typeinfo>
#include <string_view>
#include <cassert>
#include <boost/core/demangle.hpp>

using std::string;
using std::string_view;
using boost::core::scoped_demangled_name;

template<class T>
const string type_name = []{
    constexpr string_view prefix = "stanly::";
    const scoped_demangled_name type_name_of_T{typeid(T).name()};
    const string_view v{type_name_of_T.get()};

    assert((string_view{begin(v), begin(v) + prefix.size()}) == prefix);
    return string{begin(v) + prefix.size(), end(v)};
}();


// can write "convert to string" generically as non-friend member of graph:
// typeid.name() as default for name (specialize template for better names),
// to_tuple(...) to make struct into tuple, tuple_size(...) to get number of {}-placeholders, 'tuple-algorithm' to apply
// idx_to_name. then format: graph -> auto = format (map to_str graph.syntax_)

template <> struct fmt::formatter<stanly::DeclareLocalVar> : formatter<string_view> {
  template <class FormatContext>
  auto format(stanly::DeclareLocalVar n, FormatContext& ctx) const {
    return formatter<string_view>::format("(DeclareLocalVar {})", n.var, ctx);
  }
};


// string show(const FirstOrderGraph &graph) {
//   return transform_reduce(

//       begin(graph), end(graph), string{},
//       [](str str1, str str2) { return str1 + str2; },
//       []const auto &syntax) {
//         return visit([&](const auto &node) { return show(node, get); }, syntax);
//       });
// }


// string show(const DeclareLocalVar &n, GetVariable get) {
//   return format("(DeclareLocalVar {})", get(n.var));
// }
// string show(const SetField &n, GetVariable get) {
//   return format(
//       "(SetField {}[{}]={})", get(n.target), get(n.field), get(n.rhs));
// }
// string show(const LoadField &n, GetVariable get) {
//   return format(
//       "(LoadField {}={}[{}])", get(n.lhs), get(n.source), get(n.field));
// }
// string show(const LoadText &n, GetVariable get) {
//   return format("(LoadText {}='{}')", get(n.lhs), n.text_literal);
// }
// string show(const LoadRecord &n, GetVariable get) {
//   return format("(LoadRecord {}={})", get(n.lhs), n.record_literal);
// }
// string show(const LoadVar &n, GetVariable get) {
//   return format("(LoadVar {}={})", get(n.lhs), get(n.rhs));
// }

// string show(const FirstOrderGraph &graph) {
//   auto get = [&](const auto v) { return graph.var_idx_to_name(v); };
//   return transform_reduce(

//       begin(graph), end(graph), string{},
//       [](str str1, str str2) { return str1 + str2; },
//       [&](const auto &syntax) {
//         return visit([&](const auto &node) { return show(node, get); }, syntax);
//       });
// }
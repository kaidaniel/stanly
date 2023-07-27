#pragma once

#include <cstddef>
#include <forward_list>
#include <map>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "handle.h"
#include "syntax.h"

namespace stanly {
class string_index {
  // all_lit_references_: set because long strings slow to hash (?redex)
  // program_source_lits_: adding to a forward list won't invalidate references.
  // insert_lit_reference: bounds checked, <= std::numeric_limits<Idx>::max()
  // handle_to_lit_reference: Not bounds checked
  std::map<std::string_view, handle> string_view_to_handle_;
  std::vector<std::string_view> handle_to_string_view_;
  std::forward_list<std::string> strings_;

 public:
  [[nodiscard]] std::map<handle, std::string_view>
  handles() const {
    std::map<handle, std::string_view> out{};
    for (const auto& [sv, handle] : string_view_to_handle_) { out[handle] = sv; }
    return out;
  }
  string_index(const std::map<std::string_view, handle>&);
  string_index();
  handle insert(std::string_view);
  [[nodiscard]] std::string_view get_sv(handle) const;
  std::string_view add_string_to_index(std::string&&);
  node set_handles(node& node, const std::vector<std::string_view>& args);
  template <class T, std::convertible_to<std::string_view>... Args>
    requires(std::tuple_size_v<decltype(to_tpl(std::declval<T>()))> == sizeof...(Args))
  T
  make(Args&&... args) {
    return {insert(std::forward<Args>(args))...};
  }
};
extern string_index global_string_index;
handle operator""_h(const char* str, std::size_t);

std::string resolve_handles(const node&);
std::vector<std::string> resolve_handles(const std::vector<node>&);
std::map<std::string, std::string> resolve_handles(
    const std::unordered_map<std::string_view, node>&);
std::vector<node> parse(std::string&&, string_index&);
}  // namespace stanly

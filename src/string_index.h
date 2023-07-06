#pragma once

#include <forward_list>
#include <map>
#include <string_view>
#include <vector>

#include "handle.h"

namespace stanly {
class string_index {
  // all_lit_references_: set because long strings slow to hash (?redex)
  // program_source_lits_: adding to a forward list won't invalidate references.
  // insert_lit_reference: bounds checked, <= std::numeric_limits<Idx>::max()
  // handle_to_lit_reference: Not bounds checked
  std::map<std::string_view, handle> string_view_to_handle_{};
  std::vector<std::string_view> handle_to_string_view_{};
  std::forward_list<std::string> strings_{};

 public:
  string_index(const std::map<std::string_view, handle>& m) {
    std::map<handle, std::string_view> inverted_map{};
    for (const auto [sv, handle] : m) { inverted_map[handle] = sv; }
    stanly_assert(static_cast<size_t>(inverted_map.rbegin()->first) == (m.size() - 1));
    for (const auto [idx, sv] : inverted_map) {
      insert(sv);
      stanly_assert(get_sv(idx) == sv);
    }
  }
  string_index() = default;
  handle insert(std::string_view sv) {
    auto [it, did_insert] =
        string_view_to_handle_.insert({sv, handle{handle_to_string_view_.size()}});
    if (did_insert) { handle_to_string_view_.push_back(sv); }
    stanly_assert(handle_to_string_view_.size() == string_view_to_handle_.size());
    return it->second;
  };
  std::string_view get_sv(handle handle) {
    return handle_to_string_view_[static_cast<size_t>(handle)];
  };
  std::string_view add_string_to_index(std::string string) {
    strings_.push_front(std::move(string));
    return {*strings_.begin()};
  };
};
// handle operator""_h(const char* str, std::size_t size);
}  // namespace stanly
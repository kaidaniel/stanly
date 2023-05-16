#pragma once

#include <sys/types.h>

#include <forward_list>
#include <iostream>
#include <map>
#include <ranges>
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
  std::map<std::string_view, idx> string_view_to_idx_{};
  std::vector<std::string_view> idx_to_string_view_{};
  std::forward_list<std::string> strings_{};

 public:
  idx insert(std::string_view sv) {
    auto [it, did_insert] = string_view_to_idx_.insert({sv, idx{idx_to_string_view_.size()}});
    if (did_insert) { idx_to_string_view_.push_back(sv); }
    stanly_assert(idx_to_string_view_.size() == string_view_to_idx_.size());
    return it->second;
  };
  std::string_view get_sv(idx idx) { return idx_to_string_view_[static_cast<size_t>(idx)]; };
  std::string_view add_string_to_index(std::string string) {
    strings_.push_front(std::move(string));
    return {*strings_.begin()};
  };
};

template <class VariantT, class UnpackedVariantT>
class graph {
  // TODO: make parser return the context of each node so that it can be recorded here.
  std::unordered_map<size_t, std::string_view> syntax_node_idx_to_source_text_{};
  std::vector<VariantT> syntax_nodes_{};
  StringIndex string_index_{};

 public:
  auto view_syntax() {
    auto get = [this](idx i) { return string_index_.get_sv(i); };
    auto unpack = map_to_same_name<VariantT, UnpackedVariantT>(get);
    return syntax_nodes_ | std::ranges::views::transform(unpack);
  }
  graph(const std::function<std::vector<UnpackedVariantT>(std::string_view)> &parse,
        const std::function<std::string()> &read_program) {
    auto insert_var_or_record = [this](auto &&x) {
      using type = std::decay_t<decltype(x)>;
      if constexpr (std::same_as<type, std::string_view>) {
        return string_index_.insert(x);
      } else {
        static_assert(std::same_as<type, std::vector<std::string_view>>);
        // TODO: add record to index and return its index
      }
    };
    std::ranges::transform(parse(string_index_.add_string_to_index(read_program())),
                           std::back_inserter(syntax_nodes_),
                           map_to_same_name<UnpackedVariantT, VariantT>(insert_var_or_record));
  };
  graph(const graph &) = delete;
  graph(graph &&) = delete;
  graph operator=(graph &&) = delete;
  graph operator=(const graph &) = delete;
  ~graph() = default;
};

}  // namespace stanly

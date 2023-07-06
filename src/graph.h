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
#include "string_index.h"
#include "syntax.h"

namespace stanly {

template <class VariantT, class UnpackedVariantT>
class graph {
  // TODO: make parser return the conlit of each node so that it can be recorded here.
  std::unordered_map<size_t, std::string_view> syntax_node_handle_to_source_lit_{};
  std::vector<VariantT> syntax_nodes_{};
  StringIndex string_index_{};

 public:
  auto view_syntax() {
    auto get = [this](handle i) { return string_index_.get_sv(i); };
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

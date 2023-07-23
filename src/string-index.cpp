#include "string-index.h"

#include <variant>

namespace stanly {
string_index::string_index(const std::map<std::string_view, handle>& m) {
  std::map<handle, std::string_view> inverted_map{};
  for (const auto [sv, handle] : m) { inverted_map[handle] = sv; }
  stanly_assert(static_cast<size_t>(inverted_map.rbegin()->first) == (m.size() - 1));
  for (const auto [idx, sv] : inverted_map) {
    insert(sv);
    stanly_assert(get_sv(idx) == sv);
  }
}
string_index::string_index() = default;
handle
string_index::insert(std::string_view sv) {
  auto [it, did_insert] =
      string_view_to_handle_.insert({sv, handle{handle_to_string_view_.size()}});
  if (did_insert) { handle_to_string_view_.push_back(sv); }
  stanly_assert(handle_to_string_view_.size() == string_view_to_handle_.size());
  return it->second;
};
std::string_view
string_index::get_sv(handle handle) const {
  return handle_to_string_view_[static_cast<size_t>(handle)];
};
std::string_view
string_index::add_string_to_index(std::string&& string) {
  strings_.push_front(std::move(string));
  return {*strings_.begin()};
};

ast_node
string_index::set_handles(ast_node& node, const std::vector<std::string_view>& args) {
  std::visit(
      [&]<class T>(T& n) {
        // clang-format off
        if constexpr (requires(handle h) { n = {h, h, h, h, h, h, h, h}; }) {
          n = {insert(args[0]), insert(args[1]), insert(args[2]), insert(args[3]), insert(args[4]), insert(args[5]), insert(args[6]), insert(args[7])};
        } else if constexpr (requires(handle h) { n = {h, h, h, h, h, h, h}; }) {
          n = {insert(args[0]), insert(args[1]), insert(args[2]), insert(args[3]), insert(args[4]), insert(args[5]), insert(args[6])};
        } else if constexpr (requires(handle h) { n = {h, h, h, h, h, h}; }) {
          n = {insert(args[0]), insert(args[1]), insert(args[2]), insert(args[3]), insert(args[4]), insert(args[5])};
          // clang-format on
        } else if constexpr (requires(handle h) { n = {h, h, h, h, h}; }) {
          n = {insert(args[0]), insert(args[1]), insert(args[2]), insert(args[3]), insert(args[4])};
        } else if constexpr (requires(handle h) { n = {h, h, h, h}; }) {
          n = {insert(args[0]), insert(args[1]), insert(args[2]), insert(args[3])};
        } else if constexpr (requires(handle h) { n = {h, h, h}; }) {
          n = {insert(args[0]), insert(args[1]), insert(args[2])};
        } else if constexpr (requires(handle h) { n = {h, h}; }) {
          n = {insert(args[0]), insert(args[1])};
        } else {
          static_assert(requires(handle h) { n = {h}; });
          n = {insert(args[0])};
        }
      },
      node);
  return node;
}

string_index global_string_index{};

handle
operator""_h(const char* str, std::size_t size) {
  return global_string_index.insert(std::string_view{str, size});
}

template <class... Ts>
auto
map_tpl(auto&& f, std::tuple<Ts...> tpl) {
  return std::apply([&f](Ts... t) { return std::tuple{f(std::forward<Ts>(t))...}; }, tpl);
}

std::string
resolve_handles(const ast_node& node, const string_index& idx) {
  return std::visit(
      [&]<class T>(const T& n) {
        auto elements_tpl = map_tpl([&](const handle& h) { return idx.get_sv(h); }, to_tpl(n));
        return std::format("{}{}", stanly::type_name<T>, elements_tpl);
      },
      node);
}
std::string
resolve_handles(const ast_node& node) {
  return resolve_handles(node, global_string_index);
}

std::vector<std::string>
resolve_handles(const std::vector<ast_node>& nodes, const string_index& idx) {
  std::vector<std::string> out;
  out.reserve(nodes.size());
  for (const auto& node : nodes) { out.push_back(resolve_handles(node, idx)); }
  return out;
}
std::vector<std::string>
resolve_handles(const std::vector<ast_node>& nodes) {
  return resolve_handles(nodes, global_string_index);
}

std::map<std::string, std::string>
resolve_handles(
    const std::unordered_map<std::string_view, ast_node>& map, const string_index& idx) {
  std::map<std::string, std::string> out;
  // out.reserve(map.size());
  for (const auto& [key, value] : map) { out[std::string{key}] = resolve_handles(value, idx); }
  return out;
}
std::map<std::string, std::string>
resolve_handles(const std::unordered_map<std::string_view, ast_node>& map) {
  return resolve_handles(map, global_string_index);
}

}  // namespace stanly

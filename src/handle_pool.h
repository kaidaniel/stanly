#include <map>
#include <vector>

#include "syntax.h"

namespace stanly {

class handle_pool {
  mutable std::map<handle, std::string_view> handle_to_str{};
  mutable std::map<std::string_view, handle> str_to_handle{};
  handle get_handle(std::string_view sv) const {
    if (!str_to_handle.contains(sv)) {
      auto h = handle{str_to_handle.size()};
      str_to_handle[sv] = h;
      handle_to_str[h] = sv;
    };
    return str_to_handle[sv];
  }

 public:
  friend handle operator""_h(const char* str, std::size_t size);
  [[nodiscard]] const std::map<handle, std::string_view>& handles() const { return handle_to_str; }
  [[nodiscard]] const std::map<std::string_view, handle>& variables() const {
    return str_to_handle;
  }
};

static const handle_pool handle_pool{};

handle operator""_h(const char* str, std::size_t size) {
  return handle_pool.get_handle(std::string_view{str, size});
}

template <class... Ts>
auto map_tpl(auto&& f, std::tuple<Ts...> tpl) {
  return std::apply([&f](Ts... t) { return std::tuple{f(std::forward<Ts>(t))...}; }, tpl);
}

std::string resolve_handles(const syntax::ast_node& node,
                            const std::map<handle, std::string_view>& handle_to_str) {
  return std::visit(
      [&]<class T>(const T& n) {
        auto elements_tpl =
            map_tpl([&](const handle& h) { return handle_to_str.at(h); }, to_tpl(n));
        return std::format("{}{}", stanly::type_name<T>, elements_tpl);
      },
      node);
}

std::vector<std::string> resolve_handles(const std::vector<syntax::ast_node>& nodes,
                                         const std::map<handle, std::string_view>& handle_to_str) {
  std::vector<std::string> out;
  out.reserve(nodes.size());
  for (const auto& node : nodes) { out.push_back(resolve_handles(node, handle_to_str)); }
  return out;
}

std::map<std::string, std::string> resolve_handles(
    const std::unordered_map<std::string_view, syntax::ast_node>& map,
    const std::map<handle, std::string_view>& handle_to_str) {
  std::map<std::string, std::string> out;
  // out.reserve(map.size());
  for (const auto& [key, value] : map) {
    out[std::string{key}] = resolve_handles(value, handle_to_str);
  }
  return out;
}
}  // namespace stanly
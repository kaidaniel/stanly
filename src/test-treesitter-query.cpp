#include <tree_sitter/api.h>

#include <format>
#include <iostream>
#include <ranges>
#include <span>
#include <string>

extern "C" {
TSLanguage* tree_sitter_python(void);
}
namespace views = std::ranges::views;
auto read_stdin() -> std::string {
  auto program = std::string{};
  for (std::string line; std::getline(std::cin, line);) {
    program += line;
    program += '\n';
  };
  return program;
}
template <class T>
  requires std::same_as<T, TSParser> || std::same_as<T, TSTree> || std::same_as<T, TSQuery> ||
           std::same_as<T, TSQueryCursor>
struct std::default_delete<T> {
  void operator()(T* ptr) const {
    if constexpr (std::same_as<T, TSParser>) {
      ts_parser_delete(ptr);
    } else if constexpr (std::same_as<T, TSTree>) {
      ts_tree_delete(ptr);
    } else if constexpr (std::same_as<T, TSQueryCursor>) {
      ts_query_cursor_delete(ptr);
    } else {
      static_assert(std::same_as<T, TSQuery>);
      ts_query_delete(ptr);
    }
  }
};

auto parse_query(std::string query_string) {
  uint32_t error_offset{};
  TSQueryError error_type{};
  auto query = std::unique_ptr<TSQuery>{ts_query_new(
      tree_sitter_python(), query_string.c_str(), query_string.size(), &error_offset, &error_type)};
  if (error_type != 0u) { exit(error_type); }
  return [query = std::move(query)]<class G = int, class QueryCursor = int>(
             const TSNode& node, G&& call_match = 0, QueryCursor && query_cursor = 0)
      ->std::vector<std::tuple<TSNode, std::string_view>> {
    TSQueryCursor* cursor = nullptr;
    std::unique_ptr<TSQueryCursor> cursor_uptr = nullptr;
    if constexpr (std::same_as<QueryCursor, int>) {
      cursor_uptr = std::unique_ptr<TSQueryCursor>{ts_query_cursor_new()};
      cursor = cursor_uptr.get();
    } else {
      static_assert(std::same_as<QueryCursor, TSQueryCursor*>);
      cursor = query_cursor;
    }
    TSQueryMatch match{};
    ts_query_cursor_exec(cursor, query.get(), node);
    auto call_capture_ = [&](const TSQueryCapture& capture) {
      uint32_t length{};
      const char* cstr = ts_query_capture_name_for_id(query.get(), capture.index, &length);
      return std::tuple(capture.node, std::string_view{cstr, length});
    };
    std::vector<std::tuple<TSNode, std::string_view>> vec;
    while (ts_query_cursor_next_match(cursor, &match)) {
      if constexpr (std::invocable<G, const TSQueryMatch&>) { call_match(match); }
      for (const TSQueryCapture& capture : std::span{match.captures, match.capture_count}) {
        vec.emplace_back(call_capture_(capture));
      }
    }
    return vec;
  };
}

void print_capture_count(const TSQueryMatch& match) {
  std::cout << std::format("match #{}: {} captures; pattern_index: {}\n", match.id,
                           match.capture_count, match.pattern_index);
}

int main() {
  auto parser = std::unique_ptr<TSParser>{ts_parser_new()};
  ts_parser_set_language(parser.get(), tree_sitter_python());
  const auto program = read_stdin();
  auto tree = std::unique_ptr<TSTree>(
      ts_parser_parse_string(parser.get(), nullptr, program.c_str(), program.size()));
  auto root = ts_tree_root_node(tree.get());

  std::cout << std::format("{}\n{}\n\n", program,
                           std::unique_ptr<char>{ts_node_string(root)}.get());
  auto get_captures = parse_query(
      //"(module (function_definition name: (identifier) @var body: (block (return_statement (_)
      //@rhs))))"
      "(module (expression_statement (assignment left: (identifier) @var right: (identifier) "
      "@rhs)))"
      "(module (expression_statement (assignment left: (identifier) @var right: (dictionary) "
      "@rhs)))");

  auto show_capture = [&](std::tuple<TSNode, std::string_view> tpl) {
    const auto& [node, name] = tpl;
    std::cout << std::string_view{program.begin() + ts_node_start_byte(node),
                                  program.begin() + ts_node_end_byte(node)}
              << "\t";
    return std::format("{:8}{}\n", name, std::unique_ptr<char>(ts_node_string(node)).get());
  };
  auto res = get_captures(root, print_capture_count) | views::transform(show_capture);
  std::ranges::for_each(res, [](auto&& s) { std::cout << s; });
}
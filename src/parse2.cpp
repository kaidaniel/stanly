#include <array>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "parser-symbols.h"
#include "stanly-assert.h"
#include "stanly-concepts.h"  // IWYU pragma: keep (clangd bug with concepts: https://github.com/llvm/llvm-project/issues/60702)
#include "stanly-utils.h"

namespace stanly {
#define JUMP_TABLE_MAX_SIZE 250

struct field_ref {
  field field;
  std::string_view location;
};

struct symbol_ref {
  symbol symbol;
  std::string_view location;
};

template <std::size_t N>
struct tag {};

void
visit_children(cursor_c auto& cursor, auto&& f) {
  if (goto_child(cursor)) {
    do {
      auto field = current_field(cursor);
      f(field ? *field : current_symbol(cursor), current_text(cursor));
    } while (goto_sibling(cursor));
  }
  goto_parent(cursor);
}

template <parser_c Parser>
struct parse_symbol_functions {
  template <std::size_t N>
  static void
  function(Parser& parser) {
    if constexpr (requires(field f, symbol s) {
                    parse_symbol(parser.program, f, std::string_view{}, tag<N>{});
                    parse_symbol(parser.program, s, std::string_view{}, tag<N>{});
                  }) {
      visit_children(parser.cursor, [&](std::variant<field, symbol> x, std::string_view text) {
        return parse_symbol(parser.program, x, text, tag<N>{});
      });
    } else if constexpr (requires(std::vector<field_ref> vf, std::vector<symbol_ref> vs) {
                           parse_symbol(parser.program, vf, vs, std::string_view{}, tag<N>{});
                         }) {
      std::vector<field_ref> vf = {};
      std::vector<symbol_ref> vs = {};
      std::string_view text = current_text(parser.cursor);
      visit_children(parser, [&](std::variant<field, symbol> x, std::string_view text) {
        std::visit(
            [&]<class T>(T a) {
              if constexpr (std::same_as<T, field>) {
                vf.emplace_back(a, text);
              } else if constexpr (std::same_as<T, symbol>) {
                vs.emplace_back(a, text);
              }
              unreachable("variant<field, symbol> alternatives exhausted.");
            },
            x);
      });
      parse_symbol(parser.program, vf, vs, text, tag<N>{});
    } else {
      visit_children(parser.cursor, []<class T>(T, std::string_view) {});
    }
  }
};

template <std::size_t N, class FunctionPointers>
constexpr auto jump_table = [] {
  return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
    FunctionPointers f{};
    std::array<std::decay_t<decltype(f.template function<0>)>, N> arr{};
    ((arr[Is] = &f.template function<Is>), ...);
    return [=](std::size_t idx) {
      stanly_assert(idx <= N, std::format("jump_table index out of bounds: {} > {}", idx, N));
      stanly_assert(idx >= 0, std::format("jump table index negative: {} (len: {})", idx, N));
      return arr[idx];
    };
  }(std::make_index_sequence<N>{});
};

template <parser_c Parser>
void
recursively_descend(Parser& parser) {
  if (goto_child(parser.cursor)) {
    do {
      jump_table<JUMP_TABLE_MAX_SIZE, parse_symbol_functions<Parser>>(
          current_symbol(parser.cursor))(parser);
      recursively_descend(parser);
    } while (goto_sibling(parser.cursor));
  }
}

template <parser_c Parser>
decltype(Parser::program)
parse(std::string&& source) {
  Parser parser{std::move(source)};
  recursively_descend(parser);
  return parser.program;
};

}  // namespace stanly

// michaely bassios turin tech
//  evo ml
//  artemis
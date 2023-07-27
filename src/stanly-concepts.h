#pragma once

#include <__concepts/constructible.h>

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "to_tpl.h"

template <class T>
concept program_c =
    std::default_initializable<T> && requires(T::objects obj, std::string source, T t) {
      std::visit(
          []<class Object>(Object n) {
            std::apply(
                [](auto... args) {
                  T t;
                  construct<Object>(t, ((void)args, std::string_view{})...);
                },
                to_tpl(n));
          },
          obj);
      { add_string_to_index(t, std::move(source)) } -> std::same_as<std::string_view>;
    };

template <class T>
concept cursor_c = std::constructible_from<T, std::string_view> && requires(T t) {
  { current_text(t) } -> std::same_as<std::string_view>;
  { current_symbol(t) } -> std::same_as<uint16_t>;
  { current_field(t) } -> std::same_as<std::optional<uint16_t>>;
  { goto_child(t) ? 1 : 0 };
  { goto_sibling(t) ? 1 : 0 };
  { goto_parent(t) };
};

template <class T>
concept parser_c = std::constructible_from<T, std::string&&> && requires(T t) {
  requires program_c<decltype(t.program)>;
  requires cursor_c<decltype(t.cursor)>;
};

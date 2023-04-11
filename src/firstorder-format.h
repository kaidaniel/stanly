#include <boost/core/demangle.hpp>
#include <cstdio>
#include <format>
#include <functional>
#include <numeric>
#include <ranges>
#include <string_view>
#include <variant>
#include <vector>

#include "firstorder-syntax.h"

template <class T>
const std::string type_name = [] {
  const boost::core::scoped_demangled_name type_name_of_T{typeid(T).name()};
  const std::string_view v{type_name_of_T.get()};
  std::string ret;
  using size = std::string_view::size_type;
  size left = 0;
  size right = 0;
  size offset = 0;

  while (left < v.size()) {
    right = v.find_first_of("<>,", left);
    right = v.find_first_not_of("<>,", right);
    right = right == std::string_view::npos ? v.size() : right;
    offset = v.rfind("::", right);
    offset = offset == std::string_view::npos ? left : (offset + 2);
    offset = offset > left ? offset : left;
    ret += v.substr(offset, right - offset);
    left = right + 1;
  }
  return ret;
}();

template <class T>
const std::string type_name_suffix = [] {
  const boost::core::scoped_demangled_name type_name_of_T{typeid(T).name()};
  const std::string_view v{type_name_of_T.get()};
  return v.substr(std::max(v.find_last_of('>'), v.find_last_of(':') + 1)).data();
}();

auto to_tpl(auto &&object) noexcept {
  using type = std::decay_t<decltype(object)>;
  if constexpr (requires(type t) { type{{}, {}, {}, {}}; }) {
    auto &&[p1, p2, p3, p4] = object;
    return std::make_tuple(p1, p2, p3, p4);
  } else if constexpr (requires(type t) { type{{}, {}, {}}; }) {
    auto &&[p1, p2, p3] = object;
    return std::make_tuple(p1, p2, p3);
  } else if constexpr (requires(type t) { type{{}, {}}; }) {
    auto &&[p1, p2] = object;
    return std::make_tuple(p1, p2);
  } else if constexpr (requires(type t) { type{{}}; }) {
    auto &&[p1] = object;
    return std::make_tuple(p1);
  } else {
    return std::make_tuple();
  }
}

template <class T, class CharT>
struct std::formatter<std::vector<T>, CharT> : std::formatter<T, CharT> {
  std::formatter<T, CharT> unit{};
  template <class FormatContext>
  auto format(const std::vector<T> &vec, FormatContext &ctx) const {
    auto out = ctx.out();
    std::format_to(out, "{}", '[');
    for (auto it = vec.begin(); it != vec.end(); ++it) {
      unit.format(*it, ctx);
      if ((it + 1) == vec.end()) { break; }
      std::format_to(out, "{}", ", ");
    }
    return std::format_to(out, "{}", ']');
  }
};
template <class T>
concept syntax_node = stanly::contains<stanly::firstorder::syntax<std::string_view>, T>;

template <syntax_node N, class CharT>
struct std::formatter<N, CharT> : std::formatter<std::string_view, CharT> {
  template <class Ctx>
  auto format(const N &n, Ctx &ctx) const {
    auto send = [out = ctx.out()]<class T>(const T &x) { return std::format_to(out, "{}", x); };
    return std::apply(
        [&](const auto &tpl_head, const auto &...tpl_tail) {
          send(type_name_suffix<N>);
          send("(");
          send(tpl_head);
          ((send(" "), send(tpl_tail)), ...);
          return send(")");
        },
        to_tpl(n));
  }
};
template <class... Ts, class CharT>
  requires(syntax_node<Ts> && ...)
struct std::formatter<std::variant<Ts...>, CharT> : std::formatter<std::string_view> {
  template <class FormatContext>
  auto format(const stanly::firstorder::syntax<std::string_view>::node &node,
              FormatContext &ctx) const {
    std::format_to(ctx.out(), "inj-");  // variants are like Σ-types, introduced by inj.
    return std::visit(
        [&ctx]<class Node>(const Node &n) { return std::formatter<Node>{}.format(n, ctx); }, node);
  }
};

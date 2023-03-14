#pragma once

#include "iterator.h"
#include "metaprogramming.h"
#include <variant>

namespace stanly {
using idx = uint16_t;
template <class T> concept language = requires {
  typename T::template typelist<void>;
  typename T::template nodes<void, void>;
};
template <language L, typename Repr>
using syntax = metaprogramming::rebind_t<
    std::variant, typename L::template typelist<Repr>>;

template <language L, typename Repr> class get_syntax {
  using syntax = syntax<L, Repr>;
  using typelist = typename L::template typelist<Repr>;
  using nodes = typename L::template nodes<Repr>;
};

template <language L> using printable = get_syntax<L, std::string_view>;

template <language L> using packed = get_syntax<L, idx>;

template <language L> using typelist = typename L::typelist;

template <language L> using record = typename L::nodes::record;

template <language L>
typename iterator::inpt_range<printable<L>> parse(std::string_view);

} // namespace stanly

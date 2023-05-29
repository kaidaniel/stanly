#include "analyse.h"

#include <variant>
#include <vector>

#include "domain.h"
#include "syntax.h"

namespace stanly {
namespace detail {
std::ostream& operator<<(std::ostream& os, RowVarEls rve) {
  switch (rve) {
    case RowVarEls::Bot: os << "Bot"; break;
    case RowVarEls::Closed: os << "Closed"; break;
    case RowVarEls::Open: os << "Open"; break;
  };
  return os;
}

}  // namespace detail
template <class T>
struct analysis {
  using domain = domain<T>;
  using addresses = domains<T>::addresses;
  using scope = domains<T>::scope;
  using memory = domains<T>::memory;
  using object = domains<T>::object;
  using type = domains<T>::type;
  using data = domains<T>::data;
  using constant = domains<T>::constant;
  using alloc = lang<T>::alloc;
  using lit = lang<T>::lit;
  using ref = lang<T>::ref;
  using load = lang<T>::load;
  using update = lang<T>::update;
  using firstorder = lang<T>::firstorder;

  static void analyse(const alloc& alloc, domain* d) {
    d->template set_key<scope>(alloc.var, addresses{alloc.var});
    d->template set_key<memory>(alloc.var, object{{type{alloc.type}, data::bottom()}});
  }
  static void analyse(const lit& lit, domain* d) {
    d->template set_key<scope>(lit.var, addresses{lit.var});
    d->template set_key<memory>(lit.var, object{{type{lit.type}, data{constant{lit.value}}}});
  }
  static void analyse(const ref& ref, domain* d) {
    d->template set_key<scope>(ref.var, addresses{ref.src});
  }
  static void analyse(const load& load, domain* d) {
    d->template set_key<scope>(load.var, addresses{load.var});
    d->add_used_field(load.var, load.field);
  }
  static void analyse(const update& update, domain* d) {
    d->define_field(update.src, update.field, update.tgt);
  }
  domain analyse(const std::vector<firstorder>& graph) {
    domain domain;
    for (const auto& node : graph) {
      std::visit([&](const auto& n) { analyse(n, &domain); }, node);
    }
    return domain;
  }
};
domain<std::string_view> analyse(const std::vector<nodes::firstorder>& graph) {
  return analysis<std::string_view>{}.analyse(graph);
}

}  // namespace stanly
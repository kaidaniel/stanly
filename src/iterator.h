#pragma once

#include <cstddef>
#include <functional>
#include <iterator>

namespace stanly::iterator {
class iterator_sentinel {};

template <class Return> class inpt_iterator {
public:
  template <class Self>
  inpt_iterator(
      Self *self, Return (Self::*generate)(),
      bool (Self::*is_exhausted)() const)
      : generate_{[self, generate] { return (self->*generate)(); }},
        is_exhausted_(
            [self, is_exhausted] { return (self->*is_exhausted)(); }) {}
  using iterator_concept = std::input_iterator_tag;
  using difference_type = std::ptrdiff_t;
  using reference_type = Return &;
  using value_type = Return;
  inpt_iterator &operator++() {
    return_slot_ = generate_();
    return *this;
  }
  inpt_iterator operator++(int) {
    inpt_iterator copy{*this};
    ++*this;
    return copy;
  }
  const Return &operator*() const { return return_slot_; }
  friend bool
  operator==(const inpt_iterator &left, const inpt_iterator &right) {
    return left.self_ == right.self_;
  }
  friend bool operator==(const inpt_iterator &self, iterator_sentinel) {
    return self.is_exhausted_();
  }
private:
  std::function<Return()> generate_;
  std::function<bool()> is_exhausted_;
  Return return_slot_{};
};

template <typename Domain> class NodeRef {
public:
  template <typename Node>
  NodeRef(Node const &node)
      : node_{std::addressof(node)},
        transfer_{[](void const *node, Domain &d) {
          transfer(*static_cast<Node const *>(node), d);
        }} {}
private:
  friend void transfer(NodeRef const &node, Domain &d) {
    node.transfer_(node.node_, d);
  }
  using Transfer = void(void const *);
  Transfer *transfer_{nullptr};
  void const *node_{nullptr};
};

template <class Derived, class Return> class inpt_range {
  using iter = inpt_iterator<Return>;
  using sentinel_type = iterator_sentinel;
  iter iter_;
public:
  inpt_range(
      Return (Derived::*generate)(), bool (Derived::*is_exhausted)() const)
      : iter_{&static_cast<Derived &>(*this), generate, is_exhausted} {}
  [[nodiscard]] iter begin() const { return iter_; }
  [[nodiscard]] iterator_sentinel end() const { return iterator_sentinel{}; }
};

}; // namespace stanly::iterator
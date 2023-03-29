#pragma once

#include <fmt/format.h>

#include <cstddef>
#include <functional>
#include <iterator>
#include <string_view>
#include <vector>

namespace stanly::iterator {
class iterator_sentinel {};
template <class Return>
class inpt_iterator {
 public:
  template <class Self>
  inpt_iterator(Self *self, Return (Self::*generate)(), bool (Self::*is_exhausted)() const)
      : self_{self},
        is_exhausted{[self, is_exhausted] { return (self->*is_exhausted)(); }},
        generate{[self, generate] { return (self->*generate)(); }} { (*this)++; }
  using iterator_concept = std::input_iterator_tag;
  using difference_type = std::ptrdiff_t;
  using reference_type = Return &;
  using value_type = Return;
  inpt_iterator &operator++() {
    return_slot_ = generate();
    return *this;
  }
  inpt_iterator operator++(int) {
    inpt_iterator copy{*this};
    ++*this;
    return copy;
  }
  const Return &operator*() const { return return_slot_; }
  friend bool operator==(const inpt_iterator &left, const inpt_iterator &right) {
    return left.self_ == right.self_;
  }
  friend bool operator==(const inpt_iterator &self, iterator_sentinel) {
    return self.is_exhausted();
  }

 private:
  void *self_;
  std::function<bool()> is_exhausted;
  std::function<Return()> generate;
  Return return_slot_{};
};

struct exhausted {
  [[nodiscard]] static bool is_exhausted() { return true; };
  static int generate() { throw "unreachable"; };
};

template <class Return>
class inpt_range {
  using iter = inpt_iterator<Return>;
  using sentinel_type = iterator_sentinel;
  void *self_;
  iter iter_;
  void (*destroy)(void *);

 public:
  template <class Self, class... Args>
  inpt_range(Return (Self::*generate)(), bool (Self::*is_exhausted)() const, Args &&...args)
      : self_{new Self{std::forward<Args>(args)...}},
        iter_{static_cast<Self *>(self_), generate, is_exhausted},
        destroy{[](void *s) { delete static_cast<Self *>(s); }} {}
  [[nodiscard]] iter begin() const { return iter_; }
  [[nodiscard]] iterator_sentinel end() const { return iterator_sentinel{}; }
  ~inpt_range() { destroy(self_); }
  
  inpt_range(const inpt_range &other)
      : self_{other.self_}, iter_{other.iter_}, destroy{[](void *) {}} {};
  inpt_range(inpt_range &&other) noexcept
      : destroy{other.destroy}, self_{other.self_}, iter_{other.iter_} {
    other.destroy = [](void *) {};
    other.self_ = exhausted{};
    other.iter_ = {other.self_, &exhausted::generate, &exhausted::is_exhausted};
  };
  inpt_range &operator=(const inpt_range &other) {
    destroy(self_);
    self_ = other.self_;
    iter_ = other.iter_;
    destroy = [](void *) {};
  }
  inpt_range &operator=(inpt_range &&other) noexcept {
    destroy(self_);
    destroy = other.destroy;
    other.destroy = [](void *) {};
    self_ = other.self_;
    other.self_ = exhausted{};
    iter_ = other.iter_;
    other.iter_ = {other.self_, &exhausted::generate, &exhausted::is_exhausted};
  }

  // TODO add copy / move constructors (e.g. for use in fmt library)
};

};  // namespace stanly::iterator


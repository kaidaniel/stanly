#include <iterator>
#include <functional>
#include <cstddef>


namespace stanly::iterator {
class iterator_sentinel{};

template<class Self, class Return>
class input_iter {
    using SelfRef = Self*;
    public:
    input_iter(
        SelfRef self, 
        Return(Self::*generate)(),
        bool(Self::*is_exhausted)()
        ) : self_(self), generate_{generate}, is_exhausted_(is_exhausted) {}
    using iterator_concept = std::input_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using reference_type = Return&;
    using value_type = Return;
    input_iter& operator++(){ 
        return_slot_ = (self_->*generate_)();
        return *this;
    }
    input_iter operator++(int) {
        input_iter copy{*this};
        ++*this;
        return copy;
    }
    const Return& operator*() const {
        return return_slot_;
    }
    friend bool operator==(const input_iter& left, const input_iter& right) { 
        return std::addressof(left.self_) == std::addressof(right.self_) 
        and left.return_slot_ == right.return_slot_;}
    friend bool operator==(const input_iter& self, iterator_sentinel) { return (self.self_->*self.is_exhausted_)();}

    private:
    SelfRef self_;
    Return(Self::*generate_)();
    bool(Self::*is_exhausted_)();
    Return return_slot_{};
};

template<class Derived, class Return>
class input_range_adaptor {
    using iter = input_iter<Derived, Return>;
    using sentinel_type = iterator_sentinel;
    iter iter_;
    public:
    input_range_adaptor(Return(Derived::*generate)(), bool(Derived::*is_exhausted)()) 
      : iter_{&static_cast<Derived&>(*this), generate, is_exhausted} {}
    [[nodiscard]]  iter begin() const { return iter_; }
    [[nodiscard]]  iterator_sentinel end() const { return iterator_sentinel{}; }

};

};
#pragma once

#include <memory>
#include <string>

namespace ksar {

namespace detail {
  struct InterfaceBase {
    virtual ~InterfaceBase() = default;
    InterfaceBase() = default;
    InterfaceBase(const InterfaceBase &) = delete;
    InterfaceBase &operator=(const InterfaceBase &) = delete;
    InterfaceBase(InterfaceBase &&) = delete;
    InterfaceBase &operator=(InterfaceBase &&) = delete;
  };
} // namespace detail

class Analysis {
private:
  struct Interface : detail::InterfaceBase {
    [[nodiscard]] virtual std::string do_show() const = 0;
  };
  template <class T> struct Model : Interface {
    Model(const T &t) : t_(t) {}
    [[nodiscard]] std::string do_show() const override { return show(t_); }
  private:
    const T &t_;
  };
  std::unique_ptr<Interface> interface_;
  friend std::string show(const Analysis &a) { return a.interface_->do_show(); }
public:
  template <class T>
  requires requires(T t) {
    { show(t) } -> std::same_as<std::string>;
  }
  Analysis(T t) : interface_{std::make_unique<Model<T>>(std::move(t))} {}
};

class Graph {
private:
  struct Interface : detail::InterfaceBase {
    [[nodiscard]] virtual std::string do_show() const = 0;
    [[nodiscard]] virtual Analysis do_analyse() const = 0;
  };
  template <class T> struct Model : Interface {
    Model(const T &t) : t_(t) {}
    [[nodiscard]] std::string do_show() const override { return show(t_); }
    [[nodiscard]] Analysis do_analyse() const override { return analyse(t_); }
  private:
    const T &t_;
  };
  std::unique_ptr<Interface> interface_;
  friend std::string show(const Graph &g) { return g.interface_->do_show(); }
  friend Analysis analyse(const Graph &g) { return g.interface_->do_analyse(); }
public:
  template <class T>
  requires requires(T t) {
    { show(t) } -> std::same_as<std::string>;
    { analyse(t) } -> std::same_as<Analysis>;
  }
  Graph(T t) : interface_{std::make_unique<Model<T>>(std::move(t))} {}
};
Graph parse(const std::string &);

} // namespace ksar
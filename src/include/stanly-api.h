#pragma once

#include <functional>
#include <iostream>
#include <memory>
#include <string>

namespace stanly {

namespace implements {
  struct InterfaceBase {
    virtual ~InterfaceBase() = default;
    InterfaceBase() = default;
    InterfaceBase(const InterfaceBase &) = delete;
    InterfaceBase &operator=(const InterfaceBase &) = delete;
    InterfaceBase(InterfaceBase &&) = delete;
    InterfaceBase &operator=(InterfaceBase &&) = delete;
  };

  struct Show {
    struct Interface : InterfaceBase {
      [[nodiscard]] virtual std::string do_show() const = 0;
    };
    template <class T> struct Model : Show::Interface {
      Model(T &&t) : t_(std::forward<T>(t)) {}
      [[nodiscard]] std::string do_show() const override { return show(t_); }
    private:
      T t_;
    };
  };
  template <class Result> struct ShowAndAnalyse {
    struct Interface : InterfaceBase {
      [[nodiscard]] virtual std::string do_show() const = 0;
      [[nodiscard]] virtual Result do_analyse() const = 0;
    };
    template <class T, class... Args> struct Model : Interface {
      Model(T (*parse)(Args...), Args... args)
          : t_(parse(std::forward<Args>(args)...)) {}
      [[nodiscard]] std::string do_show() const override { return show(t_); }
      [[nodiscard]] Result do_analyse() const override { return analyse(t_); }
    private:
      T t_;
    };
  };
} // namespace implements

class Analysis : public implements::Show {
  using Interface = implements::Show::Interface;
  template <class T> using Model = implements::Show::Model<T>;

  std::unique_ptr<Interface> interface_;
  friend std::string show(const Analysis &a) { return a.interface_->do_show(); }
public:
  template <class T>
  requires requires(T t) { {std::string{show(t)}}; }
  explicit Analysis(T &&t)
      : interface_{std::make_unique<Model<T>>(std::forward<T>(t))} {}
};

class Graph : public implements::ShowAndAnalyse<Analysis> {
  using Interface = implements::ShowAndAnalyse<Analysis>::Interface;
  template <class T, class... Args>
  using Model = implements::ShowAndAnalyse<Analysis>::Model<T, Args...>;

  std::unique_ptr<Interface> interface_;
  friend std::string show(const Graph &g) { return g.interface_->do_show(); }
  friend Analysis analyse(const Graph &g) { return g.interface_->do_analyse(); }
public:
  template <class T, class... Args>
  requires requires(T t) {
    {std::string{show(t)}};
    {Analysis{analyse(t)}};
  }
  explicit Graph(T (*parse)(Args...), Args... args)
      : interface_{std::make_unique<Model<T, Args...>>(
            parse, std::forward<Args>(args)...)} {}
};
} // namespace stanly
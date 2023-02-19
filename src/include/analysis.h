#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace ksar {

class Analysis {
private:
  struct Interface {
    [[nodiscard]] virtual std::string do_show() const = 0;
    virtual ~Interface() = default;
    Interface() = default;
    Interface(const Interface &) = delete;
    Interface &operator=(const Interface &) = delete;
    Interface(Interface &&) = delete;
    Interface &operator=(Interface &&) = delete;
  };
  std::unique_ptr<Interface> model_;
  template <class T> struct Model : public Interface {
  public:
    Model(const T &t) : t_(t) {}
    [[nodiscard]] std::string do_show() const override { return show(t_); };
  private:
    const T &t_;
  };
  // hidden friend: can only be looked up via ADL (?)
  friend std::string show(const Analysis &a) { return a.model_->do_show(); }
public:
  template <class T>
  requires requires(T t) {
    { show(t) } -> std::same_as<std::string>;
  }
  explicit Analysis(T t) : model_(std::make_unique<Model<T>>(std::move(t))) {}
};

class Graph {
private:
  struct Interface { // ~ 'type class' in Haskell
    [[nodiscard]] virtual std::string do_show() const = 0;
    [[nodiscard]] virtual Analysis do_analyse() const = 0;
    virtual ~Interface() = default;
    Interface() = default;
    Interface(const Interface &) = delete;
    Interface &operator=(const Interface &) = delete;
    Interface(Interface &&) = delete;
    Interface &operator=(Interface &&) = delete;
  };
  std::unique_ptr<Interface> model_;
  template <class T>
  class Model : public Interface { // ~ '(type) instance' in Haskell
  public:
    Model(const T &t) : t_(t) {}
    [[nodiscard]] std::string do_show() const override { return show(t_); }
    [[nodiscard]] Analysis do_analyse() const override { return analyse(t_); }
  private:
    const T &t_;
  };
  friend std::string show(const Graph &g) { return g.model_->do_show(); }
  friend Analysis analyse(const Graph &g) { return g.model_->do_analyse(); }
public:
  template <class T>
  requires requires(T t) {
    { show(t) } -> std::same_as<std::string>;
    { analyse(t) } -> std::same_as<Analysis>;
  }
  explicit Graph(T t) : model_{std::make_unique<Model<T>>(std::move(t))} {}
};
Graph parse(const std::string &);
} // namespace ksar
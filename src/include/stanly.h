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

  struct Show {
    struct Interface : InterfaceBase {
      [[nodiscard]] virtual std::string do_show() const = 0;
    };
    template <class T> struct Model : Show::Interface {
      Model(const T &t) : t_(t) {}
      [[nodiscard]] std::string do_show() const override { return show(t_); }
    private:
      const T &t_;
    };
  };
  template <class Result> struct Analyse {
    struct Interface : InterfaceBase {
      [[nodiscard]] virtual std::string do_show() const = 0;
      [[nodiscard]] virtual Result do_analyse() const = 0;
    };
    template <class T> struct Model : Interface {
      Model(const T &t) : t_(t) {}
      [[nodiscard]] std::string do_show() const override { return show(t_); }
      [[nodiscard]] Result do_analyse() const override { return analyse(t_); }
    private:
      const T &t_;
    };
  };

} // namespace detail

class Analysis : public detail::Show {
  using Show = detail::Show;

  std::unique_ptr<Show::Interface> interface_;
  friend std::string show(const Analysis &a) { return a.interface_->do_show(); }
public:
  template <class T>
  explicit Analysis(T t)
      : interface_{std::make_unique<Show::Model<T>>(std::move(t))} {}
};

class Graph : public detail::Analyse<Analysis> {
  using Analyse = detail::Analyse<Analysis>;

  std::unique_ptr<Analyse::Interface> interface_;
  friend std::string show(const Graph &g) { return g.interface_->do_show(); }
  friend Analysis analyse(const Graph &g) { return g.interface_->do_analyse(); }
public:
  template <class T>
  explicit Graph(T t)
      : interface_{std::make_unique<Analyse::Model<T>>(std::move(t))} {}
};

Graph parse(const std::string &);

} // namespace ksar
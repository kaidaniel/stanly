#pragma once

#include <functional>
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
      Model(const T &t) : t_(t) {}
      [[nodiscard]] std::string do_show() const override { return show(t_); }
    private:
      const T &t_;
    };
  };
  template <class Result> struct ShowAndAnalyse {
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
} // namespace implements

class AnalysisType : public implements::Show {
  using Show = implements::Show;

  std::unique_ptr<Show::Interface> interface_;
  friend std::string show(const AnalysisType &a) {
    return a.interface_->do_show();
  }
public:
  template <class T>
  requires requires(T t) {
    { show(t) } -> std::same_as<std::string>;
  }
  explicit AnalysisType(T t)
      : interface_{std::make_unique<Show::Model<T>>(std::move(t))} {}
};

class GraphType : public implements::ShowAndAnalyse<AnalysisType> {
  using ShowAndAnalyse = implements::ShowAndAnalyse<AnalysisType>;

  std::unique_ptr<ShowAndAnalyse::Interface> interface_;
  friend std::string show(const GraphType &g) {
    return g.interface_->do_show();
  }
  friend AnalysisType analyse(const GraphType &g) {
    return g.interface_->do_analyse();
  }
public:
  template <class T>
  requires requires(T t) {
    { show(t) } -> std::same_as<std::string>;
    {AnalysisType{analyse(t)}};
  }
  explicit GraphType(T t)
      : interface_{std::make_unique<ShowAndAnalyse::Model<T>>(std::move(t))} {}
};
} // namespace stanly
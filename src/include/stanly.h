#pragma once

#include <memory>
#include <string>

namespace ksar {

namespace interfaces {
  struct InterfaceBase {
    virtual ~InterfaceBase() = default;
    InterfaceBase() = default;
    InterfaceBase(const InterfaceBase &) = delete;
    InterfaceBase &operator=(const InterfaceBase &) = delete;
    InterfaceBase(InterfaceBase &&) = delete;
    InterfaceBase &operator=(InterfaceBase &&) = delete;
  };

    struct ShowInterface : InterfaceBase {
    [[nodiscard]] virtual std::string do_show() const = 0;
  };
  template <class T> struct ShowModel : ShowInterface {
    ShowModel(const T &t) : t_(t) {}
    [[nodiscard]] std::string do_show() const override { return show(t_); }
  private:
    const T &t_;
  };

  template<class Interface, template<class> class Model>
  class TypeErasedInterface {
  protected:
    std::unique_ptr<Interface> interface_;
  public:
    template <class T>
    TypeErasedInterface(T t) : interface_{std::make_unique<Model<T>>(std::move(t))} {}
  };

  using Show = TypeErasedInterface<ShowInterface, ShowModel>;


  template<class AnalyseResult>
    struct AnalyseInterface : InterfaceBase{
    [[nodiscard]] virtual std::string do_show() const = 0;
    [[nodiscard]] virtual AnalyseResult do_analyse() const = 0;
  };
  template <class T, class R> struct AnalyseModel : AnalyseInterface<R> {
    AnalyseModel(const T &t) : t_(t) {}
    [[nodiscard]] std::string do_show() const override { return show(t_); }
    [[nodiscard]] R do_analyse() const override { return analyse(t_); }
  private:
    const T &t_;
  };

} // namespace interfaces

class Analysis : public interfaces::Show {
  friend std::string show(const Analysis &self) { return self.interface_->do_show(); }
};

namespace interfaces {
template<class T> using AnalyseModelOfAnalysis = AnalyseModel<T, Analysis>;
using Analyse = TypeErasedInterface<AnalyseInterface<Analysis>, AnalyseModelOfAnalysis>;
}

class Graph : public interfaces::Analyse {
  friend std::string show(const Graph &self) { return self.interface_->do_show(); }
  friend Analysis analyse(const Graph &self) { return self.interface_->do_analyse(); }
};

Graph parse(const std::string &);

} // namespace ksar
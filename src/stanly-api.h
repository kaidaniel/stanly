#pragma once

#include <functional>
#include <iostream>
#include <memory>
#include <string>

#include "fmt/format.h"

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
    [[nodiscard]] virtual std::string do_format() const = 0;
  };
  template <class T>
  struct Model : Show::Interface {
    Model(T &&t) : t_(std::forward<T>(t)) {}
    [[nodiscard]] std::string do_format() const override { return show(t_); }

   private:
    T t_;
  };
};
template <class Result>
struct ShowAndAnalyse {
  struct Interface : InterfaceBase {
    [[nodiscard]] virtual std::string do_format() const = 0;
    [[nodiscard]] virtual Result do_analyse() const = 0;
  };
  template <class T, class... Args>
  struct Model : Interface {
    Model(T (*parse)(Args...), Args... args) : t_{parse(args...)} {}
    [[nodiscard]] std::string do_format() const override { return fmt::format("{}", t_); }
    [[nodiscard]] Result do_analyse() const override { return analyse(t_); }

   private:
    T t_;
  };
};
}  // namespace implements

class Analysis : public implements::Show {
  using Interface = implements::Show::Interface;
  template <class T>
  using Model = implements::Show::Model<T>;

  std::unique_ptr<Interface> interface_;
  std::string do_format() { return interface_->do_format(); }

 public:
  template <class T>
    requires requires(T t) {
      {
        std::string { show(t) }
      };
    }
  explicit Analysis(T &&t) : interface_{std::make_unique<Model<T>>(std::forward<T>(t))} {}
};

class Graph : public implements::ShowAndAnalyse<Analysis> {
  using Interface = implements::ShowAndAnalyse<Analysis>::Interface;
  template <class T, class... Args>
  using Model = implements::ShowAndAnalyse<Analysis>::Model<T, Args...>;

  std::unique_ptr<Interface> interface_;
  std::string do_format() { return interface_->do_format(); }
  friend Analysis analyse(const Graph &g) { return g.interface_->do_analyse(); }

 public:
  template <class T, class... Args>
    requires requires(T t) {
      {
        std::string { fmt::format(t) }
      };
      {
        Analysis { analyse(t) }
      };
    }
  explicit Graph(const std::function<T(Args &&...)> &make_graph, Args &&...args)
      : interface_{std::make_unique<Model<T, Args...>>(make_graph, std::forward<Args>(args)...)} {}

  Graph (*(make_parser)(std::string_view language))(std::string_view program);
};
}  // namespace stanly

template <>
struct fmt::formatter<stanly::Graph> : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const stanly::Graph &graph, FormatContext &ctx) const {
    return formatter<string_view>::format("{}", graph, ctx);
  }
};

template <>
struct fmt::formatter<stanly::Analysis> : fmt::formatter<string_view> {
  template <class FormatContext>
  auto format(const stanly::Analysis &analysis, FormatContext &ctx) const {
    return formatter<string_view>::format("{}", analysis, ctx);
  }
};
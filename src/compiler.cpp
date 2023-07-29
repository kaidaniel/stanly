#include "compiler.h"
#include "assembler.h"
#include "tree-sitter-ast.h"

template <class T>
// clang-format off
concept assembler = std::default_initializable<T> && requires(T::ir_node obj, std::string source, T t) {
      std::visit([]<class IrNode>(IrNode n) {std::apply([](auto... args) {
          T t; construct<IrNode>(t, ((void)args, std::string_view{})...);},to_tpl(n));}, obj);
      { add_string_to_index(t, std::move(source)) } -> std::same_as<std::string_view>;
};
// clang-format on
template <class T>
concept compiler = std::constructible_from<T, std::string&&> && requires(T t, T::tree_node n) {
  typename T::tree_node;
  requires assembler<decltype(t.assembler)>;
  requires tree<decltype(t.tree), typename T::tree_node>;
  node_kind(n)->std::integral;
};

namespace stanly {
template <compiler Compiler>
decltype(Compiler::assembler)
parse(std::string&& source) {
  using tree_node = typename Compiler::tree_node;
  using fns = visit_tree_node_functions<tree_node, decltype(Compiler::program)>;
  Compiler compiler{std::move(source)};
  std::vector<tree_node> children = {};
  std::vector<std::size_t> n_children = {0};
  tarverse_tree(
      compiler.cursor,
      [&](tree_node&& v) {  // called in pre-order
        children.push_back(std::move(v));
        n_children.back() += 1;
        n_children.push_back(0);
      },
      [&](tree_node&& v) {  // called in post-order
        std::size_t n_args = n_children.back();
        n_children.pop_back();
        stanly_assert(
            children.size() >= (n_args + 1),
            std::format(
                "not enough children to visit node: expected {}, but got only {}", n_args + 1,
                children.size()));
        std::span<tree_node> args = {children.end() - n_args, children.end()};
        auto symbol = std::invoke(compiler.node_kind, std::move(v));
        jump_table<JUMP_TABLE_MAX_SIZE, fns>(symbol)(compiler.assembler, args, args.data() - 1);
        children.erase(children.end() - n_args, children.end());
      });
  return compiler.assembler;
};
}
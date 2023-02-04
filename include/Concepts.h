#ifndef KSAR_SPARTA_CONCEPTS_H
#define KSAR_SPARTA_CONCEPTS_H

#include <concepts>
#include <ranges>
#include <type_traits>

namespace sparta {
using namespace std;

template <typename>
class AbstractDomain;

template <typename>
class AbstractValue;

enum class AbstractValueKind;

template <typename T>
concept Value = derived_from<T, AbstractValue<T>> &&
    is_copy_constructible<T>::value && requires(T x, const T &y) {
  {T{}};
  {x = y};
  {x.clear()};
  { x.kind() } -> same_as<AbstractValueKind>;
  { x.leq(y) } -> same_as<bool>;
  { x.equals(y) } -> same_as<bool>;
  { x.join_with(y) } -> same_as<AbstractValueKind>;
  { x.widen_with(y) } -> same_as<AbstractValueKind>;
  { x.meet_with(y) } -> same_as<AbstractValueKind>;
  { x.narrow_with(y) } -> same_as<AbstractValueKind>;
};

template <typename T>
concept GraphInterface = requires(T, const typename T::Graph g,
                                  const typename T::EdgeId &e,
                                  const typename T::NodeId &n) {
  { T::entry(g) } -> same_as<typename T::NodeId>;
  { T::source(g, e) } -> same_as<typename T::NodeId>;
  { T::predecessors(g, n) } -> ranges::range<>;
  { T::successors(g, n) } -> ranges::range<>;
};

template <typename T>
concept Domain = derived_from<T, AbstractDomain<T>> &&
    is_copy_constructible<T>::value && requires(T x, const T &y) {
  {T{}};
  {x = y};
  { x.is_bottom() } -> same_as<bool>;
  { x.is_top() } -> same_as<bool>;
  {x.set_to_bottom()};
  {x.set_to_top()};
  {x.join_with(y)};
  {x.widen_with(y)};
  {x.meet_with(y)};
  {x.narrow_with(y)};
};

// domain transformers
template <Domain... T>
class DisjointUnion;
template <Domain... T>
class DirectProduct;
template <Domain T>
class Reverse;
template <Domain T>
class Lifted;

// partition: implicitly BOTTOM (-> iterate over other.items())
// (abstraction of) Union of execution paths, indexed by Label
// Android: (field : Container x Name x Type) -> DisjointUnion String Integer
// HeapPointer ...
// => union of all paths with "load field", abstracted as "result register"
template <typename Label, Domain D>
class HashedPartition;
// environment: implicitly TOP (-> iterate over this.items())
// intersection of all possible states
template <typename Label, Domain D>
class TreePartition;
template <typename Variable, Domain D>
class HashedEnvironment;

// simple domains
template <typename T>
class Constant;
template <typename E /*Enum*/, typename L, typename C, L *l>
class Finite;
class IntegerSet;  // widens to Top unless one is subset of the other
template <integral N>
class Interval;

template <Value T>
class Scaffolding;

template <typename T, typename G, typename D>
concept FixpointIter = GraphInterface<G> && Domain<D> &&
    requires(const typename G::NodeId &n, const typename G::EdgeId &e, D d) {
  T::analyze_node(n, &d);
  T::analyze_edge(e, d)->same_as<D>;
};

template <typename T>
concept Registry = requires(T t) {
  { t.has_update() } -> same_as<bool>;
  {t.materialize_update()};
};

template <typename T, typename F, typename R, typename C, typename G>
concept FunctionAnalyzer = Registry<R> &&
    requires(T *t, F &f, R *r, C *c, G *g, void *par) {
  T(f);
  t->set_summaries(r);
  t->set_caller_context(c);
  t->set_call_graph(g);
  t->set_analysis_parameters(par);
  t->analyze();
};

template <typename T, typename G, typename D>
concept FixpointIteratorInterface = Domain<D> && requires(const G &g) {
  T(g);
  { T::initial_domain() } -> same_as<D>;
};

template <typename T, typename FA, typename FP>
concept AnalysisT =
    GraphInterface<typename T::CallGraphInterface> &&
    Domain<typename T::Callsite::Domain> && Registry<typename T::Registry> &&
    FunctionAnalyzer < typename T::template FunctionAnalyzer<FA>,
typename T::Function, typename T::Registry, typename T::Callsite::Domain,
    typename T::CallGraphInterface::Graph >
        &&FixpointIteratorInterface<
            typename T::template FixpointIteratorBase<
                FA, typename T::CallGraphInterface::Graph,
                typename T::Callsite::Domain>,
            typename T::CallGraphInterface::Graph, typename T::Callsite::Domain>
            &&requires(typename T::CallGraphInterface::NodeId &n,
                       typename T::Program p, typename T::Registry r) {
  { T::function_by_node_id(n) } -> same_as<typename T::Function &>;
  { T::call_graph_of(p, &r) } -> same_as<typename T::CallGraphInterface::Graph>;
};

}  // namespace sparta

#endif  // KSAR_SPARTA_CONCEPTS_H

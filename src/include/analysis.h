#include <string>
#include <variant>
#include <vector>

namespace ksar {

class Graph {
public:
  Graph(const std::string &);
  std::string show();

  ~Graph() = default;
  Graph(const Graph &) = delete;
  Graph &operator=(const Graph &) = delete;
  Graph(Graph &&) = delete;
  Graph &operator=(Graph &&) = delete;
};

class Analysis {
public:
  Analysis(const Graph &);
  std::string show();

  ~Analysis() = default;
  Analysis(const Analysis &) = delete;
  Analysis &operator=(const Analysis &) = delete;
  Analysis(Analysis &&) = delete;
  Analysis &operator=(Analysis &&) = delete;
};

Graph parse(const std::string &);
Analysis analyse(const Graph &);
} // namespace ksar
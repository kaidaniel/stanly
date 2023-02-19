namespace ksar{
class FirstOrderLanguageGraph;
class FirstOrderLanguageAnalysis;

Graph parse_first_order_language(const std::string &);
Analysis analyse(const FirstOrderLanguageGraph&);
std::string show(const FirstOrderLanguageGraph&);
std::string show(const FirstOrderLanguageAnalysis&);
}
namespace ksar{
class FirstOrderLanguageGraph;
class FirstOrderLanguageAnalysis;

Analysis analyse(const FirstOrderLanguageGraph&);
std::string show(const FirstOrderLanguageGraph&);
std::string show(const FirstOrderLanguageAnalysis&);
}
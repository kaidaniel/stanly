[[ $1 ]] && nodes_json="$1" || nodes_json="build-default/tree-sitter-python/src/node-types.json"
[[ $2 ]] && lookup_symbols="$2" || lookup_symbols="build-default/src/lookup-symbols"

function preamble {
[[ ${1##*.} == "hpp" ]] && echo "# pragma once" > $1 || echo "#include \"$header_file\"" > $1

cat << EOF >> $1

// generated using "generate_parser_symbols.sh"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::$(echo $(basename ${1%.*}) | tr - _) {

EOF
}

function postamble {
echo "}" >> $1
clang-format -i $1
sed -i '/{}/d' $1
clang++ -fsyntax-only $1 -Wall -Werror
clang-tidy $1
}

non_terminals=$(jq -r -c '.[] | select(has("children") or (has("fields") and (.fields | length !=0)) or has("subtypes"))' < $nodes_json)

# ------ generate src/parser.hpp -------- #
header_file="parser.hpp"
preamble "src/$header_file"
preamble "src/${header_file%.*}.cpp"

echo $(date +'%D %H:%M:%S') symbols terminals fields

cat << EOF >> "src/$header_file"
enum class terminals;
struct parser;
void parse(parser&, enum terminals);

enum class symbols {
$(jq -r '.[] | select(.named).type' < $nodes_json | $lookup_symbols)};

enum class terminals {
$(jq -r '.[] | select(.named and .children == null and .subtypes == null and ((.fields == null) or (.fields | length == 0))).type' < $nodes_json | $lookup_symbols)};

enum class fields {
$(jq -r ".[] | select(.named and has(\"fields\")) | select(.fields | length != 0).fields | keys[]" < $nodes_json | $lookup_symbols fields)};
EOF

for symbol in $non_terminals; do
    name=$(jq -r '.type' <<< $symbol) 
    echo $(date +'%D %H:%M:%S') $name
    subtypes="$(jq -r '.subtypes' <<< $symbol)"
    children="$(jq -r '.children.types' <<< $symbol)"
    fields="$(jq -r '.fields' <<< $symbol)"
    [[ "$fields" == "{}" ]] && fields="null"
    cat << EOF >> "src/$header_file"

    namespace $name {
    void parse(parser&);
EOF
    [[ "$subtypes" != "null" ]] && cat << EOF >> "src/$header_file"
    enum class subtypes { $(jq -r '.[].type' <<< $subtypes | $lookup_symbols)};
EOF
    [[ "$children" != "null" ]] && cat << EOF >> "src/$header_file"
    enum class children { $(jq -r '.[].type'  <<< $children | $lookup_symbols)};
EOF
    if [[ "$fields" != "null" ]]; then cat << EOF >> "src/$header_file"
        enum class field_names { $(jq -r 'keys[]' <<< $fields | $lookup_symbols fields)};
        namespace fields {
EOF
        for field in $(jq -r -c 'keys[]' <<< $fields); do cat << EOF >> "src/$header_file"
            enum class $field { $(jq -r --arg fd "$field" '.[$fd].types[].type' <<< $fields | $lookup_symbols) };
EOF
        done; echo } >> "src/$header_file"
    fi; echo } >> "src/$header_file"
done

postamble "src/$header_file"
postamble "src/${header_file%.*}.cpp"


# # ------ generate src/parser.cpp -------- #

# preamble "${header_file%.*}.cpp"

# echo "void f();"

# echo "// clang-format off"
# for non_terminal in $non_terminals; do
#     name=$(echo $non_terminal | jq -r '.type')
#     echo "namespace $name { void parse(parser& p); }"
# done
# echo "// clang-format on"


# postamble
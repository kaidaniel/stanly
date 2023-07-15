[[ $1 ]] && nodes_json="$1" || nodes_json="build-default/tree-sitter-python/src/node-types.json"
[[ $2 ]] && lookup_symbols="$2" || lookup_symbols="build-default/src/lookup-symbols"

# non_terminals=$(jq -r -c '.[] | select(.named) | select(has("children") or (has("fields") and (.fields | length !=0)) or has("subtypes")) | {type: .type, subtypes: [.subtypes[]?.type], children: [.children.types[]?.type], fields: [select(.fields | length !=0).fields], field_names: [select(.fields | length !=0).fields | keys[]]}' < $nodes_json)
# terminals=$(< $nodes_json jq -r '.[] | select(.named and .children == null and .subtypes == null and ((.fields == null) or (.fields | length == 0))).type')
echo $(date +'%D %H:%M:%S') generating "src/parser.hpp" and "src/.parser_skeleton.cpp"

cat << HEADER > src/parser.hpp
#pragma once
#include <utility>
#include <string_view>

// generated using "$0"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::parser {

    enum class fields;
    struct parser;

    std::optional<std::string_view> parse_field(parser&, enum fields);
    std::optional<std::string_view> parse_children(parser&);

    enum class symbols {
        $(< $nodes_json jq -r '.[] | select(.named).type'| $lookup_symbols)
    };

    enum class fields {
        $(< $nodes_json jq -r ".[].fields | keys? | .[]" | $lookup_symbols fields)
    };

    $( for name in $(jq -r -c '.[] | select(.named).type' < $nodes_json); do
        echo "void parse_$name(parser&);"
    done)
}
HEADER

echo $(date +'%D %H:%M:%S') done generating "src/parser.hpp"

cat << SKELETON > src/.parser_skeleton.cpp
#include "parser.hpp"
#include <utility>
#include <string_view>

// generated using "$0"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::parser {

$(  for symbol in $(jq -r -c '.[] | select(.named)' < $nodes_json); do
        name="sym_"$( <<< $symbol jq -r '.type') 
        field_names=$(<<< $symbol jq -r -c '.fields | keys? | .[]')
cat << EOF
        void parse_$name(parser& p){ 
            $( [[ $field_names ]] && while read f; do echo "auto sym_$f = parse_field(p, fields::sym_$f);"; done <<< "$field_names")
            auto children = parse_children(p);
/*
$( jq '{fields: (.fields // {}) | map_values([.types[]?.type]), children: (if .children then {multiple: .children.multiple, required: .children.required, types: [.children.types[]?.type]} else null end), subtypes: [.subtypes[]?.type]} | map_values(select(length > 0))' <<< $symbol | tr -d '",{}[]' | sed '/^[[:space:]]*$/d')
*/
        }
EOF
    done)
SKELETON

echo $(date +'%D %H:%M:%S') done generating "src/.parser_skeleton.cpp"


cat << TRAMPOLINE > src/parser_trampoline.cpp
#include "parser.hpp"
#include <utility>
#include <string_view>

// generated using "$0"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::parser {

TRAMPOLINE


function lint {
    sed -i '/{}/d' $1
    clang-format -i $1
    sed -i '/{}/d' $1
    /home/kai/projects/install/bin/clang++ -stdlib=libc++ -fexperimental-library -fsyntax-only $1 -Wall -Werror -Wno-unused-variable
}
echo "}" >> src/.parser_skeleton.cpp
lint src/parser.hpp
lint src/.parser_skeleton.cpp

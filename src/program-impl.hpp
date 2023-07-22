#include "program.hpp"
#include "syntax.h"
#include "string-index.h"
#include <vector>

namespace stanly {
    struct program {
        std::vector<syntax::ast_node> nodes;
        std::vector<syntax::basic_block> basic_blocks;
        string_index* idx;
    };
    template<syntax::ast_cons T>
    void make_node(program& p, std::same_as<std::string_view> auto... args){
        p.nodes.push_back(p.idx->make<T>(args...));
    }
    template<syntax::basic_block_cons T>
    void make_basic_block(program& p, std::same_as<std::string_view> auto... args){
        p.basic_blocks.emplace_back(p.idx->make<T>(args...), {});
    };
}
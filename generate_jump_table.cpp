#include <concepts>
#include <string>
#include <variant>
#include <array>


template<std::size_t N>
struct tag {};

template<std::size_t N>
requires(N < 70)
int do_parse(int i, tag<N>) { return N + i; }


struct parse_struct {
    template<std::size_t N>
    static auto function(int i){ 
        if constexpr(requires{do_parse(i, tag<N>{});}){
            return do_parse(i, tag<N>{});
        } else {
            return 42;
        }
    }
};
constexpr parse_struct parse{};


template<std::size_t N, class CPO>
constexpr auto lookup_table(CPO&& f){
return [&]<std::size_t... Is>(std::index_sequence<Is...>){
    std::array<std::decay_t<decltype(f.template function<0>)>, N> arr {};
    ((arr[Is] = &f.template function<Is>), ...);
    return arr;
}(std::make_index_sequence<N>{});
}
inline constexpr auto fns = lookup_table<250>(parse);

int g(int i){
    return fns[i](i);
}

int main(){
    return fns[18](10) + fns[10](29);
}





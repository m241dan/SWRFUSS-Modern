#ifndef SWRGM_MATCH_H
#define SWRGM_MATCH_H

#include <variant>
#include <expected>
#include "atom_enum.h"

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
// explicit deduction guide (not needed as of C++20)
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template<typename T, typename OSet>
constexpr auto match(T&& t, OSet&& oset)
{
    return std::forward<OSet>(oset)(std::forward<T>(t));
}

template<typename... Ts, typename OSet>
constexpr auto match(std::variant<Ts...> ts, OSet&& oset)
{
    return std::visit(std::forward<OSet>(oset), ts);
}

template<typename V, typename E, typename OSet>
constexpr auto match(std::expected<V, E> e, OSet&& oset)
{
    if (e)
        return match(e.value(), std::forward<OSet>(oset));
    else
        return match(e.error(), std::forward<OSet>(oset));
}

template<typename OSet, typename... Atoms>
constexpr auto match(atom_enum<Atoms...> e, OSet&& oset)
{
    return std::visit(std::forward<OSet>(oset), e.value);
}


#endif //SWRGM_MATCH_H

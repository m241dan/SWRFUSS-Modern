#ifndef SWRGM_ATOMS_H
#define SWRGM_ATOMS_H

#include <concepts>
#include <type_traits>

template<class T>
struct atom
{
    bool constexpr operator==(const T&) const
    {
        return true;
    };

    template<class U> requires (!std::is_same_v<T, U>)
    bool constexpr operator==(const U&) const
    {
        return false;
    }
};

template<class T>
concept is_atom =
       std::is_trivially_default_constructible_v<T>
    && std::is_nothrow_default_constructible_v<T>
    && std::is_trivially_copy_constructible_v<T>
    && std::is_nothrow_copy_constructible_v<T>
    && std::is_trivially_move_constructible_v<T>
    && std::is_nothrow_move_constructible_v<T>
    && requires (atom<T> a, atom<T> b)
    {
        { a == b } -> std::convertible_to<bool>;
    };
;


#endif //SWRGM_ATOMS_H

#ifndef SWRGM_ATOM_ENUM_H
#define SWRGM_ATOM_ENUM_H

#include "atoms.h"
#include <concepts>
#include <tuple>
#include <variant>

template<class T, class... Ts>
concept in_pack = (std::same_as<Ts, T> || ...);

template<is_atom... Atoms>
struct atom_enum
{
    using type = atom_enum<Atoms...>;
    using default_variant_type = typename std::tuple_element<0, std::tuple<Atoms...>>;
    using variant_type = std::variant<Atoms...>;

    // default constructor
    constexpr atom_enum() noexcept : value{default_variant_type{}} {}

    // constructor
    template<in_pack<Atoms...> T>
    constexpr explicit atom_enum(T a) noexcept : value{a} {}

    // copy constructor
    constexpr atom_enum(const atom_enum<Atoms...>& other) noexcept : value{other.value} {}

    // move constructor
    constexpr atom_enum(atom_enum<Atoms...>&& other) noexcept : value{std::move(other.value)}
    {
        value = default_variant_type{};
    }

    // copy assignment
    atom_enum<Atoms...>& operator=(const atom_enum<Atoms...>& other)
    {
        if (this == &other)
        {
            return *this;
        }

        value = other.value;

        return *this;
    }

    // copy assignment from an atom
    template<in_pack<Atoms...> T>
    atom_enum<Atoms...>& operator=(const T& a)
    {
        value = a;

        return *this;
    }

    // move assignment
    atom_enum<Atoms...>& operator=(atom_enum<Atoms...>&& other) noexcept
    {
        if (this == &other)
        {
            return *this;
        }

        value = std::move(other.value);
        other.value = default_variant_type{};

        return *this;
    }

    //  move assignment from an atom (forwarding reference?)
    template<in_pack<Atoms...> T>
    atom_enum<Atoms...>& operator=(T&& a)
    {
        value = std::forward<T>(a);
        return *this;
    }

    // operators

    // comparison
    constexpr auto operator==(const atom_enum<Atoms...>& other) const
    {
        return value == other.value;
    }

    template<in_pack<Atoms...> T>
    constexpr auto operator==(const T& other) const
    {
        return value.index() == variant_type{other}.index();
    }

    // spaceship
    constexpr auto operator<=>(const atom_enum<Atoms...>& other) const
    {
        return value == other.value;
    }

    template<in_pack<Atoms...> T>
    constexpr auto operator<=>(const T& other) const
    {
        return value.index() <=> variant_type{other}.index();
    }

    // conversion
    constexpr explicit operator size_t() const
    {
        return value.index();
    }

    variant_type value;
};

#endif //SWRGM_ATOM_ENUM_H

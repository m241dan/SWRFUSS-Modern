#ifndef SWRGM_NAMESPACES_H
#define SWRGM_NAMESPACES_H

#include <ranges>
#include <algorithm>
#include <functional>

namespace alg {
    using namespace std::ranges;

    /**
     * Concept that extends the std::indirectly_unary_invocable to make sure what is invoked returns a bool.
     * @tparam F Type of the Function that will be invoked.
     * @tparam I Type of the arguments passed to the function of type F.
     */
    template<class F, class I>
    concept indirectly_unary_invocable_returns_bool =
           std::indirectly_unary_invocable<F, I>
        && requires(F f, I i)
        {
            { f(*i) } -> std::same_as<bool>;
        }
    ;

    /**
     * Concept that checks that a function is called with no arguments and returns no arguments.
     * @tparam F Type of the Function that will be invoked.
     */
    template<class F>
    concept invocable_void_void = requires(F f)
    {
        { f() } -> std::same_as<void>;
    };

    /**
     * A version of std::ranges::for_each that can be aborted by the interior function returning a false, and if it
     * does return a false, it will NOT run the given "else" function. However, if the loop terminates and it reached
     * the end it will call its else function.
     */
    struct for_each_else_fn
    {
        template<
            std::input_iterator I,
            std::sentinel_for<I> S,
            class Proj = std::identity,
            indirectly_unary_invocable_returns_bool<std::projected<I, Proj>> Fun,
            invocable_void_void ElseFun
        >
        constexpr std::ranges::for_each_result<I, Fun>
        operator()(I first, S last, Fun f, ElseFun else_f, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (!std::invoke(f, std::invoke(proj, *first)))
                    break;
            if (first == last)
                std::invoke(else_f);
            return {std::move(first), std::move(f)};
        }

        template<
            std::ranges::input_range R,
            class Proj = std::identity,
            indirectly_unary_invocable_returns_bool<std::projected<std::ranges::iterator_t<R>, Proj>> Fun,
            invocable_void_void ElseFun
        >
        constexpr std::ranges::for_each_result<std::ranges::borrowed_iterator_t<R>, Fun>
        operator()(R&& r, Fun f, ElseFun else_f, Proj proj = {}) const
        {
            return (*this)(std::ranges::begin(r), std::ranges::end(r), std::move(f), std::move(else_f), std::ref(proj));
        }
    };

    inline constexpr for_each_else_fn for_each_else;

    /**
     * A version of std::ranges::for_each that will call the or function if the given range is empty, otherwise it
     * performs "normally."
     */
    struct for_each_or_fn
    {
        template<
            std::input_iterator I,
            std::sentinel_for<I> S,
            class Proj = std::identity,
            std::indirectly_unary_invocable<std::projected<I, Proj>> Fun,
            invocable_void_void OrFun
        >
        constexpr std::ranges::for_each_result<I, Fun>
        operator()(I first, S last, Fun f, OrFun or_f, Proj proj = {}) const
        {
            if (first == last)
                std::invoke(or_f);
            else
                for (; first != last; ++first)
                    std::invoke(f, std::invoke(proj, *first));

            return {std::move(first), std::move(f)};
        }

        template<
            std::ranges::input_range R,
            class Proj = std::identity,
            std::indirectly_unary_invocable<std::projected<std::ranges::iterator_t<R>, Proj>> Fun,
            invocable_void_void OrFun
        >
        constexpr std::ranges::for_each_result<std::ranges::borrowed_iterator_t<R>, Fun>
        operator()(R&& r, Fun f, OrFun or_f, Proj proj = {}) const
        {
            return (*this)(std::ranges::begin(r), std::ranges::end(r), std::move(f), std::move(or_f), std::ref(proj));
        }
    };

    inline constexpr for_each_or_fn for_each_or;

    struct all_true_fn
    {
        template<std::input_iterator I, std::sentinel_for<I> S, class Proj = std::identity>
        constexpr bool operator()(I first, S last, Proj proj = {}) const
        {
            return std::ranges::find_if_not(first, last, [](bool x) {return x;}, std::ref(proj)) == last;
        }

        template<std::ranges::input_range R, class Proj = std::identity>
        constexpr bool operator()(R&& r, Proj proj = {}) const
        {
            return operator()(std::ranges::begin(r), std::ranges::end(r), std::ref(proj));
        }
    };

    inline constexpr all_true_fn all_true;
}

namespace view {
    using namespace std::views;

    template<class T>
    constexpr auto take_if(T&& pred)
    {
        return filter(std::forward<T>(pred));
    }

    template<class T>
    constexpr auto drop_if(T&& pred)
    {
        const auto not_pred = [&]<class... Args>(Args&&... args)
        {
            return !std::forward<T>(pred)(std::forward<Args>(args)...);
        };
        return filter(not_pred);
    }

    struct _lowercase : __adaptor::_RangeAdaptorClosure
    {
        constexpr auto operator() [[nodiscard]] (std::string_view r) const
        {
            return transform(r, [](char c) {return std::tolower(c);});
        }
    };

    inline constexpr _lowercase lowercase;

}

namespace ops {
    const auto decrement = [](auto& i) {i--;};
    template<class T>
    auto same_as(T&& lhs)
    {
        return [&](T&& rhs) -> bool
        {
            return std::forward<T>(lhs) == std::forward<T>(rhs);
        };
    }
    template<class T>
    auto not_same_as(T&& lhs)
    {
        return [&](T&& rhs) -> bool
        {
            return std::forward<T>(lhs) != std::forward<T>(rhs);
        };
    }
}

namespace concepts {
    template<class T>
    concept can_follow = requires(T t)
    {
        { t.master };
    };

    template<class T>
    concept has_position = requires(T t)
    {
        { t.position };
    };

    template<class T, class U>
    concept equalable = requires(T a, U b)
    {
        { a == b } -> std::convertible_to<bool>;
    };
}

namespace ops {
    template<class T> requires concepts::can_follow<std::remove_pointer_t<std::remove_cvref_t<T>>>
    constexpr auto is_follower(T&& leader)
    {
        return [&](T&& follower) -> bool
        {
            return std::forward<T>(follower)->master == std::forward<T>(leader);
        };
    }

    template<class U>
    constexpr auto in_position(U&& pos)
    {
        return [&]<class T> requires concepts::has_position<std::remove_pointer_t<std::remove_cvref_t<T>>>
        (T&& ch) -> bool
        {
            return std::forward<T>(ch)->position == std::forward<U>(pos);
        };
    }

    template<class T, class Proj = std::identity>
    constexpr auto equal_to(T&& rhs, Proj proj = {})
    {
        // The following requires clause works nice IF the types have operation== for itself.
        // - requires equalable<typename std::projected<U, Proj>::value_type, std::remove_pointer_t<std::remove_cvref_t<T>>>
        // We'll get there at some point...
        return [rhs=std::forward<T>(rhs), proj=std::move(proj)]<class U>(U&& lhs) -> bool
        {
            return rhs == std::invoke(proj, std::forward<U>(lhs));
        };
    }
};

#endif //SWRGM_NAMESPACES_H

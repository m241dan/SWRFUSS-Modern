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
}

namespace view {
    using namespace std::views;
}

namespace ops {
    const auto decrement = [](auto& i) {i--;};
}

#endif //SWRGM_NAMESPACES_H

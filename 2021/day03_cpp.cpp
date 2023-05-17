//     g++ -std=c++20 -ftemplate-depth=4096 day3.cpp
// or: clang++ -std=c++20 -ftemplate-depth=2048 day3.cpp

#include <array>
#include <cstdio>

// change to 1 for real input
// change to 0 for test input
const int TEST_OR_REAL = 1;

constexpr int SIZE = TEST_OR_REAL ? 1000 : 12;
constexpr int NUM_BITS = TEST_OR_REAL ? 12 : 5;
constexpr std::array<int, 12> input1 = {
#include "input3-1_cpp.txt"
};

constexpr std::array<int, 1000> input2 = {
#include "input3-2_cpp.txt"
};

template <int bits, int... count>
struct Step {
    template <size_t... idxs>
    static constexpr auto make(std::index_sequence<idxs...> unused) {
        return std::array{ count + bool(bits & (1 << (sizeof...(count) - idxs - 1)))... };
    }
    static constexpr auto value = make(std::make_index_sequence<sizeof...(count)>{});
};

template <int cur, std::array<int, SIZE> nums, int... count>
struct Loop {
    static constexpr auto step = Step<nums[cur], count...>::value;
    template <size_t... I>
    static constexpr auto make(std::index_sequence<I...> unused) {
        return Loop<cur+1, nums, step[I]...>::value;
    }
    static constexpr auto value = make(std::make_index_sequence<step.size()>{});
};

template <std::array<int, SIZE> nums, int... count>
struct Loop<SIZE, nums, count...> {
    static constexpr std::array<int, sizeof...(count)> value = {count...};
};

template <int cur, std::array<int, NUM_BITS> count>
struct GammaRate {
    static constexpr int value =
        (count[cur] > SIZE/2) * (1 << (NUM_BITS - cur - 1)) +
        GammaRate<cur+1, count>::value;
};

template <std::array<int, NUM_BITS> count>
struct GammaRate<NUM_BITS, count> {
    static constexpr int value = 0;
};

template <int cur, std::array<int, NUM_BITS> count>
struct EpsRate {
    static constexpr int value =
        (count[cur] < SIZE/2) * (1 << (NUM_BITS - cur - 1)) +
        EpsRate<cur+1, count>::value;
};

template <std::array<int, NUM_BITS> count>
struct EpsRate<NUM_BITS, count> {
    static constexpr int value = 0;
};

template <std::array<int, NUM_BITS> count>
struct MakeResult {
    static constexpr int value = GammaRate<0, count>::value * EpsRate<0, count>::value;
};

int main()
{
    auto a = MakeResult<Loop<0, input2, 0,0,0,0,0,0,0,0,0,0,0,0>::value>::value;
    printf("%d\n", a);
}

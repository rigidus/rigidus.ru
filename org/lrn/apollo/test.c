#define LED_PIN B, 4
#define SET_PIN(ARGS) __SET_PIN(ARGS)
#define __SET_PIN(PORT_LETTER, PIN) PORT ## PORT_LETTER |= (1 << PIN)

#define E(...) __VA_ARGS__ //Identity macro
#define N(...) //Zero macro
#define __test(something,k,...) k(do something)
#define _test(something,...) __test(something,E)

#define _MY(first, ...) first ## _MY(__VA_ARGS__)
#define MY(...) _MY(__VA_ARGS__)

# define EMPTY(...)
# define DEFER(...) __VA_ARGS__ EMPTY()
# define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()
# define EXPAND(...) __VA_ARGS__

#define EVAL(...)  EVAL1(EVAL1(EVAL1(__VA_ARGS__)))
#define EVAL1(...) EVAL2(EVAL2(EVAL2(__VA_ARGS__)))
#define EVAL2(...) EVAL3(EVAL3(EVAL3(__VA_ARGS__)))
#define EVAL3(...) EVAL4(EVAL4(EVAL4(__VA_ARGS__)))
#define EVAL4(...) EVAL5(EVAL5(EVAL5(__VA_ARGS__)))
#define EVAL5(...) __VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)
#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__

#define INC(x) PRIMITIVE_CAT(INC_, x)
#define INC_0 1
#define INC_1 2
#define INC_2 3
#define INC_3 4
#define INC_4 5
#define INC_5 6
#define INC_6 7
#define INC_7 8
#define INC_8 9
#define INC_9 9

#define DEC(x) PRIMITIVE_CAT(DEC_, x)
#define DEC_0 0
#define DEC_1 0
#define DEC_2 1
#define DEC_3 2
#define DEC_4 3
#define DEC_5 4
#define DEC_6 5
#define DEC_7 6
#define DEC_8 7
#define DEC_9 8

#define CHECK_N(x, n, ...) n
#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)

#define NOT(x) CHECK(PRIMITIVE_CAT(NOT_, x))
#define NOT_0 ~, 1,

#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)
#define COMPL_0 1
#define COMPL_1 0

#define BOOL(x) COMPL(NOT(x))

#define IIF(c) PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t

#define IF(c) IIF(BOOL(c))

#define EAT(...)
#define EXPAND(...) __VA_ARGS__
#define WHEN(c) IF(c)(EXPAND, EAT)

#define REPEAT(count, macro, ...)               \
    WHEN(count)                                 \
        (                                       \
         OBSTRUCT(REPEAT_INDIRECT) ()           \
         (                                      \
          DEC(count), macro, __VA_ARGS__        \
           )                                    \
         OBSTRUCT(macro)                        \
         (                                      \
          DEC(count), __VA_ARGS__               \
           )                                    \
          )
#define REPEAT_INDIRECT() REPEAT

#define M(i, _) i

int main () {
    EVAL(REPEAT(8, M, ~)) // 0 1 2 3 4 5 6 7
}

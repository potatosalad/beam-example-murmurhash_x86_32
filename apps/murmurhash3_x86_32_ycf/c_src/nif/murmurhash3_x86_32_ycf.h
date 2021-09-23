#ifndef YCF_YIELDING_C_FUN_HELPERS
#define YCF_YIELDING_C_FUN_HELPERS 1
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * YCF_GCC_DIAG_ON and YCF_GCC_DIAG_OFF can be used to temporarly
 * disable a gcc or clang warning in a file.
 *
 * Example:
 * YCF_GCC_DIAG_OFF(unused-function)
 * static int test(){ return 0;}
 * YCF_GCC_DIAG_ON(unused-function)
 *
 * These macros were orginally authored by Jonathan Wakely and has
 * been modified by Patrick Horgan.
 *
 * Source: http://dbp-consulting.com/tutorials/SuppressingGCCWarnings.html
 *
 */
#if defined(_MSC_VER)
#define YCF_GCC_DIAG_OFF(x) __pragma(warning(push, 0))
#define YCF_GCC_DIAG_ON(x) __pragma(warning(pop))
#elif ((__GNUC__ * 100) + __GNUC_MINOR__) >= 402
#define YCF_GCC_DIAG_STR(s) #s
#define YCF_GCC_DIAG_JOINSTR(x, y) YCF_GCC_DIAG_STR(x##y)
#define YCF_GCC_DIAG_DO_PRAGMA(x) _Pragma(#x)
#define YCF_GCC_DIAG_PRAGMA(x) YCF_GCC_DIAG_DO_PRAGMA(GCC diagnostic x)
#if ((__GNUC__ * 100) + __GNUC_MINOR__) >= 406
#define YCF_GCC_DIAG_OFF(x)                                                                                            \
    YCF_GCC_DIAG_PRAGMA(push)                                                                                          \
    YCF_GCC_DIAG_PRAGMA(ignored YCF_GCC_DIAG_JOINSTR(-W, x))
#define YCF_GCC_DIAG_ON(x) YCF_GCC_DIAG_PRAGMA(pop)
#else
#define YCF_GCC_DIAG_OFF(x) YCF_GCC_DIAG_PRAGMA(ignored YCF_GCC_DIAG_JOINSTR(-W, x))
#define YCF_GCC_DIAG_ON(x) YCF_GCC_DIAG_PRAGMA(warning YCF_GCC_DIAG_JOINSTR(-W, x))
#endif
#else
#define YCF_GCC_DIAG_OFF(x)
#define YCF_GCC_DIAG_ON(x)
#endif
#ifdef __GNUC__
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 5) || defined(__clang__)
#define YCF_GCC_ATTRIBUTE_UNUSED __attribute__((unused))
#else
#define YCF_GCC_ATTRIBUTE_UNUSED
#endif
#else
#define YCF_GCC_ATTRIBUTE_UNUSED
#endif

typedef void *(*ycf_yield_alloc_type)(size_t, void *);
typedef void (*ycf_yield_free_type)(void *, void *);

struct ycf_alloc_data {
    size_t size;
    size_t max_size;
    int needs_freeing;
    char *data;
};

#define YCF_ALLOC_NEXT_BLOCK()                                                                                         \
    (ycf_frame_alloc_data.data == NULL ? NULL : ((void *)(&ycf_frame_alloc_data.data[ycf_frame_alloc_data.size])))
#define YCF_ALLOC_NEXT_MAX_SIZE()                                                                                      \
    (ycf_frame_alloc_data.data == NULL ? ycf_frame_alloc_data.max_size                                                 \
                                       : (ycf_frame_alloc_data.max_size - ycf_frame_alloc_data.size))

/* Macros for special code sections */
#define ON_SAVE_YIELD_STATE ON_SAVE_YIELD_STATE
#define ON_RESTORE_YIELD_STATE ON_RESTORE_YIELD_STATE
#define ON_DESTROY_STATE ON_DESTROY_STATE
#define ON_RETURN ON_RETURN
#define ON_DESTROY_STATE_OR_RETURN ON_DESTROY_STATE_OR_RETURN
#define YCF_SPECIAL_CODE_START(PARAM)                                                                                  \
    /*special_code_start:PARAM*/                                                                                       \
    if (0) {
#define YCF_SPECIAL_CODE_END()                                                                                         \
    }                                                                                                                  \
    /*special_code_end*/

YCF_GCC_ATTRIBUTE_UNUSED
static void *
ycf_stack_alloc(size_t size, struct ycf_alloc_data *data, ycf_yield_alloc_type allocator,
                void *ycf_yield_alloc_free_context, size_t default_stack_size)
{
    void *ret = NULL;
    if (data->data == NULL) {
        if (default_stack_size == 0) {
            fprintf(stderr, "ycf_alloc: not enough stack!! (max size = 0)\n");
            exit(1);
        }
        data->data = allocator(default_stack_size, ycf_yield_alloc_free_context);
        data->needs_freeing = 1;
        data->max_size = default_stack_size;
        data->size = 0;
    }
    if (data->size + size > data->max_size) {
        fprintf(stderr, "ycf_alloc: not enough stack! (max size = %zu)\n", default_stack_size);
        exit(1);
    }
    ret = &data->data[data->size];
    data->size = data->size + size + (sizeof(void *) - (size % sizeof(void *))) % sizeof(void *);
    return ret;
}
YCF_GCC_ATTRIBUTE_UNUSED
static void
ycf_destroy_stack_allocator(struct ycf_alloc_data *data, ycf_yield_free_type freer, void *ycf_yield_alloc_free_context)
{
    if (data->needs_freeing) {
        freer(data->data, ycf_yield_alloc_free_context);
    }
}

#include <limits.h>
#define YCF_MAX_NR_OF_REDS LONG_MAX
#define YCF_NR_OF_REDS_LEFT() ycf_nr_of_reductions
#define YCF_SET_NR_OF_REDS_LEFT(NEW_NR_OF_REDS_LEFT)                                                                   \
    do {                                                                                                               \
        ycf_nr_of_reductions = (NEW_NR_OF_REDS_LEFT);                                                                  \
    } while (0)

#define YCF_GET_EXTRA_CONTEXT() ycf_extra_context

#define YCF_IS_YIELDED(CTX) (CTX != NULL)

#define YCF_YIELD_CODE_GENERATED 1

/* end of YCF_YIELDING_C_FUN_HELPERS guard */
#endif

struct gen_ycf_trap_state_for_murmurhash3_x86_32_update;
/* clang-format off */
YCF_GCC_DIAG_OFF(unused-function)
/* clang-format on */

void murmurhash3_x86_32_update_ycf_gen_continue(long *ycf_number_of_reduction_param, void **ycf_trap_state,
                                                void *ycf_extra_context);
/* clang-format off */
YCF_GCC_DIAG_ON(unused-function)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(unused-function)
/* clang-format on */

void murmurhash3_x86_32_update_ycf_gen_destroy(void *ycf_my_trap_state_param);
/* clang-format off */
YCF_GCC_DIAG_ON(unused-function)
/* clang-format on */

void murmurhash3_x86_32_update_ycf_gen_yielding(long *ycf_nr_of_reductions_param, void **ycf_trap_state,
                                                void *ycf_extra_context, ycf_yield_alloc_type ycf_yield_alloc,
                                                ycf_yield_free_type ycf_yield_free, void *ycf_yield_alloc_free_context,
                                                size_t ycf_stack_alloc_size_or_max_size, void *ycf_stack_alloc_data,
                                                murmurhash3_x86_32_ctx_t *ctx_N2_N6, uint8_t *key_N3_N7,
                                                size_t key_len_N4_N8);

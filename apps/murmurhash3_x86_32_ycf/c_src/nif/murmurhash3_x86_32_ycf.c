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

#include "portable_endian.h"

#ifdef YCF_YIELD_CODE_GENERATED
#include "../codegen/murmurhash3_x86_32.h"
#else
#include "murmurhash3_x86_32.h"
#endif

//-----------------------------------------------------------------------------
// Platform-specific functions and macros

// Microsoft Visual Studio

#if defined(_MSC_VER)

#define FORCE_INLINE __forceinline

#include <stdlib.h>

#define ROTL32(x, y) _rotl(x, y)

#define BIG_CONSTANT(x) (x)

// Other compilers

#else // defined(_MSC_VER)

#define FORCE_INLINE inline __attribute__((always_inline))

static uint32_t rotl32(uint32_t x, int8_t r);

inline uint32_t
rotl32(uint32_t x, int8_t r)
{
    return (x << r) | (x >> (32 - r));
}

#define ROTL32(x, y) rotl32(x, y)

#define BIG_CONSTANT(x) (x##LLU)

#endif // !defined(_MSC_VER)

//-----------------------------------------------------------------------------
// Block read - if your platform needs to do endian-swapping or can only
// handle aligned reads, do the conversion here

FORCE_INLINE uint32_t
getblock32(const uint32_t *p, int i)
{
    return le32toh(p[i]);
}

//-----------------------------------------------------------------------------
// Finalization mix - force all bits of a hash block to avalanche

FORCE_INLINE uint32_t
fmix32(uint32_t h)
{
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;

    return h;
}

/** MurmurHash3_x86_32 **/

static void murmurhash3_x86_32_update_body(murmurhash3_x86_32_ctx_t *ctx, const murmurhash3_x86_32_block_t block);
static void murmurhash3_x86_32_update_tail(murmurhash3_x86_32_ctx_t *ctx, const murmurhash3_x86_32_block_t block,
                                           uint64_t length);

void
murmurhash3_x86_32_init(murmurhash3_x86_32_ctx_t *ctx, uint32_t seed)
{
    (void)memset(ctx, 0, sizeof(murmurhash3_x86_32_ctx_t));
    ctx->h1 = seed;
    ctx->offset = 0;
    ctx->length = 0;
    return;
}

inline void
murmurhash3_x86_32_update_body(murmurhash3_x86_32_ctx_t *ctx, const murmurhash3_x86_32_block_t block)
{
    const uint32_t c1 = 0xcc9e2d51;
    const uint32_t c2 = 0x1b873593;

    uint32_t h1 = ctx->h1;
    uint32_t k1 = getblock32((const void *)block, 0);

    k1 *= c1;
    k1 = ROTL32(k1, 15);
    k1 *= c2;

    h1 ^= k1;
    h1 = ROTL32(h1, 13);
    h1 = h1 * 5 + 0xe6546b64;

    ctx->h1 = h1;

    return;
}

inline void
murmurhash3_x86_32_update_tail(murmurhash3_x86_32_ctx_t *ctx, const murmurhash3_x86_32_block_t block, uint64_t length)
{
    const uint32_t c1 = 0xcc9e2d51;
    const uint32_t c2 = 0x1b873593;

    uint32_t h1 = ctx->h1;
    uint32_t k1 = 0;

    const uint8_t *tail = (const uint8_t *)(block);

    switch (length & 3) {
    case 3:
        k1 ^= tail[2] << 16;
    case 2:
        k1 ^= tail[1] << 8;
    case 1:
        k1 ^= tail[0];
        k1 *= c1;
        k1 = ROTL32(k1, 15);
        k1 *= c2;
        h1 ^= k1;
    };

    ctx->h1 = h1;

    return;
}
#define YCF_IN_YIELDING_FUN 1
#undef YCF_STACK_ALLOC
#define YCF_STACK_ALLOC(SIZE)                                                                                          \
    ycf_stack_alloc(SIZE, &ycf_frame_alloc_data, ycf_yield_alloc, ycf_yield_alloc_free_context,                        \
                    ycf_stack_alloc_size_or_max_size)

void murmurhash3_x86_32_update_ycf_gen_yielding(long *ycf_nr_of_reductions_param, void **ycf_trap_state,
                                                void *ycf_extra_context, ycf_yield_alloc_type ycf_yield_alloc,
                                                ycf_yield_free_type ycf_yield_free, void *ycf_yield_alloc_free_context,
                                                size_t ycf_stack_alloc_size_or_max_size, void *ycf_stack_alloc_data,
                                                murmurhash3_x86_32_ctx_t *ctx_N2_N6, uint8_t *key_N3_N7,
                                                size_t key_len_N4_N8);

struct gen_ycf_trap_state_for_murmurhash3_x86_32_update {
    ycf_yield_alloc_type ycf_yield_alloc;
    ycf_yield_free_type ycf_yield_free;
    void *ycf_yield_alloc_free_context;
    size_t ycf_stack_alloc_size_or_max_size;
    void *ycf_stack_alloc_data;
    int ycf_trap_location;
    long ycf_nr_of_reductions;
    struct ycf_alloc_data ycf_frame_alloc_data;
    size_t key_off_N1_N5;
    murmurhash3_x86_32_ctx_t *ctx_N2_N6;
    uint8_t *key_N3_N7;
    size_t key_len_N4_N8;
};

/* clang-format off */
YCF_GCC_DIAG_OFF(unused-function)
/* clang-format on */

void
murmurhash3_x86_32_update_ycf_gen_continue(long *ycf_number_of_reduction_param, void **ycf_trap_state,
                                           void *ycf_extra_context)
{
    struct gen_ycf_trap_state_for_murmurhash3_x86_32_update *ycf_my_trap_state = *ycf_trap_state;
    murmurhash3_x86_32_update_ycf_gen_yielding(
        ycf_number_of_reduction_param, ycf_trap_state, ycf_extra_context, ycf_my_trap_state->ycf_yield_alloc,
        ycf_my_trap_state->ycf_yield_free, ycf_my_trap_state->ycf_yield_alloc_free_context,
        ycf_my_trap_state->ycf_stack_alloc_size_or_max_size, ycf_my_trap_state->ycf_stack_alloc_data,
        ycf_my_trap_state->ctx_N2_N6, ycf_my_trap_state->key_N3_N7, ycf_my_trap_state->key_len_N4_N8);
    return;
}
/* clang-format off */
YCF_GCC_DIAG_ON(unused-function)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(pragmas)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(unknown-warning-option)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(unused-but-set-variable)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(unused-function)
/* clang-format on */

void
murmurhash3_x86_32_update_ycf_gen_destroy(void *ycf_my_trap_state_param)
{
    {
        struct gen_ycf_trap_state_for_murmurhash3_x86_32_update *ycf_my_trap_state = ycf_my_trap_state_param;

        ycf_yield_alloc_type ycf_yield_alloc;
        ycf_yield_free_type ycf_yield_free;
        void *ycf_yield_alloc_free_context;
        size_t ycf_stack_alloc_size_or_max_size;
        void *ycf_stack_alloc_data;
        int ycf_trap_location;
        long ycf_nr_of_reductions;
        struct ycf_alloc_data ycf_frame_alloc_data;
        size_t key_off_N1_N5;
        murmurhash3_x86_32_ctx_t *ctx_N2_N6;
        uint8_t *key_N3_N7;
        size_t key_len_N4_N8;

        ycf_yield_alloc = ycf_my_trap_state->ycf_yield_alloc;
        ycf_yield_free = ycf_my_trap_state->ycf_yield_free;
        ycf_yield_alloc_free_context = ycf_my_trap_state->ycf_yield_alloc_free_context;
        ycf_stack_alloc_size_or_max_size = ycf_my_trap_state->ycf_stack_alloc_size_or_max_size;
        ycf_stack_alloc_data = ycf_my_trap_state->ycf_stack_alloc_data;
        ycf_trap_location = ycf_my_trap_state->ycf_trap_location;
        ycf_nr_of_reductions = ycf_my_trap_state->ycf_nr_of_reductions;
        ycf_frame_alloc_data = ycf_my_trap_state->ycf_frame_alloc_data;
        key_off_N1_N5 = ycf_my_trap_state->key_off_N1_N5;
        ctx_N2_N6 = ycf_my_trap_state->ctx_N2_N6;
        key_N3_N7 = ycf_my_trap_state->key_N3_N7;
        key_len_N4_N8 = ycf_my_trap_state->key_len_N4_N8;

        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        ycf_destroy_stack_allocator(&ycf_frame_alloc_data, ycf_yield_free, ycf_yield_alloc_free_context);
        ycf_yield_free(ycf_my_trap_state, ycf_yield_alloc_free_context);
    }
}
/* clang-format off */
YCF_GCC_DIAG_ON(unused-function)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(unused-but-set-variable)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(unknown-warning-option)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(pragmas)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(pragmas)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(unknown-warning-option)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(sometimes-uninitialized)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(maybe-uninitialized)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_OFF(uninitialized)
/* clang-format on */

void
murmurhash3_x86_32_update_ycf_gen_yielding(long *ycf_nr_of_reductions_param, void **ycf_trap_state,
                                           void *ycf_extra_context, ycf_yield_alloc_type ycf_yield_alloc,
                                           ycf_yield_free_type ycf_yield_free, void *ycf_yield_alloc_free_context,
                                           size_t ycf_stack_alloc_size_or_max_size, void *ycf_stack_alloc_data,
                                           murmurhash3_x86_32_ctx_t *ctx_N2_N6, uint8_t *key_N3_N7,
                                           size_t key_len_N4_N8)
{
    size_t key_off_N1_N5;
    int ycf_trap_location;
    long ycf_nr_of_reductions;
    struct ycf_alloc_data ycf_frame_alloc_data;

    {

        ycf_nr_of_reductions = *ycf_nr_of_reductions_param;
        ycf_frame_alloc_data.size = 0;
        ycf_frame_alloc_data.max_size = ycf_stack_alloc_size_or_max_size;
        ycf_frame_alloc_data.data = ycf_stack_alloc_data;
        ycf_frame_alloc_data.needs_freeing = 0;
        if (*ycf_trap_state != NULL) {
            struct gen_ycf_trap_state_for_murmurhash3_x86_32_update *ycf_my_trap_state = *ycf_trap_state;

            ycf_yield_alloc = ycf_my_trap_state->ycf_yield_alloc;
            ycf_yield_free = ycf_my_trap_state->ycf_yield_free;
            ycf_yield_alloc_free_context = ycf_my_trap_state->ycf_yield_alloc_free_context;
            ycf_stack_alloc_size_or_max_size = ycf_my_trap_state->ycf_stack_alloc_size_or_max_size;
            ycf_stack_alloc_data = ycf_my_trap_state->ycf_stack_alloc_data;
            ycf_trap_location = ycf_my_trap_state->ycf_trap_location;
            ycf_nr_of_reductions = ycf_my_trap_state->ycf_nr_of_reductions;
            ycf_frame_alloc_data = ycf_my_trap_state->ycf_frame_alloc_data;
            key_off_N1_N5 = ycf_my_trap_state->key_off_N1_N5;
            ctx_N2_N6 = ycf_my_trap_state->ctx_N2_N6;
            key_N3_N7 = ycf_my_trap_state->key_N3_N7;
            key_len_N4_N8 = ycf_my_trap_state->key_len_N4_N8;

            /* YCF SPECIAL CUSTOM CODE START */

            /* YCF SPECIAL CUSTOM CODE END */

            ycf_nr_of_reductions = *ycf_nr_of_reductions_param;
            switch (ycf_trap_location) {
            case 1:
                goto ycf_yield_location_label_1;
            }
        }
    }
    {
    }
    key_off_N1_N5 = 0;

    if (key_len_N4_N8 == 0) {
        {

            if (*ycf_trap_state != NULL) {
                ycf_yield_free(*ycf_trap_state, ycf_yield_alloc_free_context);
                *ycf_trap_state = NULL;
            }
            ycf_destroy_stack_allocator(&ycf_frame_alloc_data, ycf_yield_free, ycf_yield_alloc_free_context);
            *ycf_nr_of_reductions_param = ycf_nr_of_reductions;
            /* YCF SPECIAL CUSTOM CODE START */

            /* YCF SPECIAL CUSTOM CODE END */

            /* YCF SPECIAL CUSTOM CODE START */

            /* YCF SPECIAL CUSTOM CODE END */

            return;
        }
    }

    if (ctx_N2_N6->offset > 0 && (ctx_N2_N6->offset + key_len_N4_N8) >= MURMURHASH3_X86_32_BLOCK_SIZE) {
        {
            key_off_N1_N5 = (MURMURHASH3_X86_32_BLOCK_SIZE - ctx_N2_N6->offset);
            (void)memcpy(&ctx_N2_N6->buffer[ctx_N2_N6->offset], key_N3_N7, key_off_N1_N5);
            (void)murmurhash3_x86_32_update_body(ctx_N2_N6, &ctx_N2_N6->buffer[0]);
            (void)memset(ctx_N2_N6->buffer, 0, MURMURHASH3_X86_32_BLOCK_SIZE);
            ctx_N2_N6->offset = 0;
            ctx_N2_N6->length += MURMURHASH3_X86_32_BLOCK_SIZE;
        }
    }

    while (key_off_N1_N5 + MURMURHASH3_X86_32_BLOCK_SIZE <= key_len_N4_N8) {
        {
            ycf_nr_of_reductions = ycf_nr_of_reductions - (1);
            if (ycf_nr_of_reductions <= 0) {
                ycf_trap_location = 1;
                goto ycf_do_yield_label_murmurhash3_x86_32_update;
            ycf_yield_location_label_1:;
            }
        }
        {
            (void)murmurhash3_x86_32_update_body(ctx_N2_N6, &key_N3_N7[key_off_N1_N5]);
            key_off_N1_N5 += MURMURHASH3_X86_32_BLOCK_SIZE;
            ctx_N2_N6->length += MURMURHASH3_X86_32_BLOCK_SIZE;
        }
    }

    if (key_off_N1_N5 < key_len_N4_N8) {
        {
            (void)memcpy(&ctx_N2_N6->buffer[ctx_N2_N6->offset], &key_N3_N7[key_off_N1_N5],
                         key_len_N4_N8 - key_off_N1_N5);
            ctx_N2_N6->offset += key_len_N4_N8 - key_off_N1_N5;
        }
    }
    {

        if (*ycf_trap_state != NULL) {
            ycf_yield_free(*ycf_trap_state, ycf_yield_alloc_free_context);
            *ycf_trap_state = NULL;
        }
        ycf_destroy_stack_allocator(&ycf_frame_alloc_data, ycf_yield_free, ycf_yield_alloc_free_context);
        *ycf_nr_of_reductions_param = ycf_nr_of_reductions;
        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        return;
    }
    {

        if (*ycf_trap_state != NULL) {
            ycf_yield_free(*ycf_trap_state, ycf_yield_alloc_free_context);
            *ycf_trap_state = NULL;
        }
        ycf_destroy_stack_allocator(&ycf_frame_alloc_data, ycf_yield_free, ycf_yield_alloc_free_context);
        *ycf_nr_of_reductions_param = ycf_nr_of_reductions;
        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        return;
    }
    {

        if (*ycf_trap_state != NULL) {
            ycf_yield_free(*ycf_trap_state, ycf_yield_alloc_free_context);
            *ycf_trap_state = NULL;
        }
        ycf_destroy_stack_allocator(&ycf_frame_alloc_data, ycf_yield_free, ycf_yield_alloc_free_context);
        *ycf_nr_of_reductions_param = ycf_nr_of_reductions;
        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */

        return;
    }

    {
        struct gen_ycf_trap_state_for_murmurhash3_x86_32_update *ycf_my_trap_state;
    ycf_do_yield_label_murmurhash3_x86_32_update:;

        /* YCF SPECIAL CUSTOM CODE START */

        /* YCF SPECIAL CUSTOM CODE END */
        if (*ycf_trap_state == NULL) {
            ycf_my_trap_state = ycf_yield_alloc(sizeof(struct gen_ycf_trap_state_for_murmurhash3_x86_32_update),
                                                ycf_yield_alloc_free_context);
        } else {
            ycf_my_trap_state = *ycf_trap_state;
        }
        ycf_my_trap_state->ycf_yield_alloc = ycf_yield_alloc;
        ycf_my_trap_state->ycf_yield_free = ycf_yield_free;
        ycf_my_trap_state->ycf_yield_alloc_free_context = ycf_yield_alloc_free_context;
        ycf_my_trap_state->ycf_stack_alloc_size_or_max_size = ycf_stack_alloc_size_or_max_size;
        ycf_my_trap_state->ycf_stack_alloc_data = ycf_stack_alloc_data;
        ycf_my_trap_state->ycf_trap_location = ycf_trap_location;
        ycf_my_trap_state->ycf_nr_of_reductions = ycf_nr_of_reductions;
        ycf_my_trap_state->ycf_frame_alloc_data = ycf_frame_alloc_data;
        ycf_my_trap_state->key_off_N1_N5 = key_off_N1_N5;
        ycf_my_trap_state->ctx_N2_N6 = ctx_N2_N6;
        ycf_my_trap_state->key_N3_N7 = key_N3_N7;
        ycf_my_trap_state->key_len_N4_N8 = key_len_N4_N8;

        *ycf_nr_of_reductions_param = ycf_nr_of_reductions;
        *ycf_trap_state = ycf_my_trap_state;
        return;
    }
}
/* clang-format off */
YCF_GCC_DIAG_ON(uninitialized)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(maybe-uninitialized)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(sometimes-uninitialized)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(unknown-warning-option)
/* clang-format on */

/* clang-format off */
YCF_GCC_DIAG_ON(pragmas)
/* clang-format on */

#undef YCF_STACK_ALLOC
#undef YCF_IN_YIELDING_FUN
#define YCF_STACK_ALLOC(SIZE) malloc(SIZE)

void
murmurhash3_x86_32_update(murmurhash3_x86_32_ctx_t *ctx, const uint8_t *key, size_t key_len)
{
    size_t key_off = 0;

    if (key_len == 0) {
        return;
    }

    if (ctx->offset > 0 && (ctx->offset + key_len) >= MURMURHASH3_X86_32_BLOCK_SIZE) {
        key_off = (MURMURHASH3_X86_32_BLOCK_SIZE - ctx->offset);
        (void)memcpy(&ctx->buffer[ctx->offset], key, key_off);
        (void)murmurhash3_x86_32_update_body(ctx, &ctx->buffer[0]);
        (void)memset(ctx->buffer, 0, MURMURHASH3_X86_32_BLOCK_SIZE);
        ctx->offset = 0;
        ctx->length += MURMURHASH3_X86_32_BLOCK_SIZE;
    }

    while (key_off + MURMURHASH3_X86_32_BLOCK_SIZE <= key_len) {
        (void)murmurhash3_x86_32_update_body(ctx, &key[key_off]);
        key_off += MURMURHASH3_X86_32_BLOCK_SIZE;
        ctx->length += MURMURHASH3_X86_32_BLOCK_SIZE;
    }

    if (key_off < key_len) {
        (void)memcpy(&ctx->buffer[ctx->offset], &key[key_off], key_len - key_off);
        ctx->offset += key_len - key_off;
    }

    return;
}
#undef YCF_STACK_ALLOC

void
murmurhash3_x86_32_final(murmurhash3_x86_32_ctx_t *ctx, murmurhash3_x86_32_block_t out)
{
    ctx->length += ctx->offset;
    (void)murmurhash3_x86_32_update_tail(ctx, &ctx->buffer[0], ctx->length);
    (void)memset(ctx->buffer, 0, ctx->offset);
    ctx->offset = 0;

    uint32_t h1 = ctx->h1;

    h1 ^= ctx->length;

    h1 = fmix32(h1);

    ((uint32_t *)out)[0] = h1;

    (void)murmurhash3_x86_32_destroy(ctx);
    return;
}

void
murmurhash3_x86_32_destroy(murmurhash3_x86_32_ctx_t *ctx)
{
    (void)memset(ctx, 0, sizeof(murmurhash3_x86_32_ctx_t));
    return;
}

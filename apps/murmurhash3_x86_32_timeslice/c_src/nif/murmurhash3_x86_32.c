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

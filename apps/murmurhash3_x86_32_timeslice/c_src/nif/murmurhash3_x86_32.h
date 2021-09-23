#ifndef MURMURHASH3_X86_32_H
#define MURMURHASH3_X86_32_H

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Public Macros */

#define MURMURHASH3_X86_32_BLOCK_SIZE (4)

/* Public Types */

/** MurmurHash3_x86_32 **/

typedef uint8_t murmurhash3_x86_32_block_t[MURMURHASH3_X86_32_BLOCK_SIZE];

typedef struct murmurhash3_x86_32_ctx_s murmurhash3_x86_32_ctx_t;

struct murmurhash3_x86_32_ctx_s {
    uint32_t h1;
    murmurhash3_x86_32_block_t buffer;
    uint32_t offset;
    uint32_t length;
};

/* Public Function Declarations */

/** MurmurHash3_x86_32 **/
extern void murmurhash3_x86_32_init(murmurhash3_x86_32_ctx_t *ctx, uint32_t seed);
extern void murmurhash3_x86_32_update(murmurhash3_x86_32_ctx_t *ctx, const uint8_t *key, size_t key_len);
extern void murmurhash3_x86_32_final(murmurhash3_x86_32_ctx_t *ctx, murmurhash3_x86_32_block_t out);
extern void murmurhash3_x86_32_destroy(murmurhash3_x86_32_ctx_t *ctx);

#ifdef __cplusplus
}
#endif

#endif

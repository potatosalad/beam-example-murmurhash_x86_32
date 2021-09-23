#include "murmurhash3_x86_32_ref_nif.h"

#include <stdbool.h>
#include <unistd.h>

#include "murmurhash3_x86_32_ref.h"

/* Static Variables */

static ERL_NIF_TERM ATOM_badarg;
static ERL_NIF_TERM ATOM_error;
static ERL_NIF_TERM ATOM_false;
static ERL_NIF_TERM ATOM_nil;
static ERL_NIF_TERM ATOM_notsup;
static ERL_NIF_TERM ATOM_ok;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_undefined;

/* All nif functions return a valid value or throws an exception */
#define EXCP(Env, Id, Str)                                                                                             \
    enif_raise_exception((Env),                                                                                        \
                         enif_make_tuple3((Env), (Id),                                                                 \
                                          enif_make_tuple2((Env), enif_make_string((Env), __FILE__, (ERL_NIF_LATIN1)), \
                                                           enif_make_int((Env), __LINE__)),                            \
                                          enif_make_string((Env), (Str), (ERL_NIF_LATIN1))))

#define EXCP_NOTSUP(Env, Str) EXCP((Env), ATOM_notsup, (Str))
#define EXCP_BADARG(Env, Str) EXCP((Env), ATOM_badarg, (Str))
#define EXCP_ERROR(Env, Str) EXCP((Env), ATOM_error, (Str))

/* NIF Function Declarations */

static ERL_NIF_TERM murmurhash3_x86_32_ref_nif_hash_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Function Definitions */

ERL_NIF_TERM
murmurhash3_x86_32_ref_nif_hash_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned long seed_ulong;
    ErlNifBinary key_bin;
    unsigned char *out_buf = NULL;
    ERL_NIF_TERM out_term;

    if (argc != 2) {
        return EXCP_NOTSUP(env, "argc must be 2");
    }
    if (!enif_get_ulong(env, argv[0], &seed_ulong) || seed_ulong > 0xFFFFFFFF) {
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }
    if (!enif_inspect_iolist_as_binary(env, argv[1], &key_bin)) {
        return EXCP_BADARG(env, "Bad argument: 'Key'");
    }

    out_buf = enif_make_new_binary(env, 4, &out_term);
    if (out_buf == NULL) {
        return EXCP_ERROR(env, "Can't allocate 'Out' binary");
    }

    (void)MurmurHash3_x86_32(key_bin.data, key_bin.size, (uint32_t)seed_ulong, out_buf);

    return out_term;
}

/* NIF Callbacks */

static ErlNifFunc murmurhash3_x86_32_ref_nif_funcs[] = {
    {"hash", 2, murmurhash3_x86_32_ref_nif_hash_2, ERL_NIF_NORMAL_JOB_BOUND},
    {"hash_dirty", 2, murmurhash3_x86_32_ref_nif_hash_2, ERL_NIF_DIRTY_JOB_CPU_BOUND},
};

static int murmurhash3_x86_32_ref_nif_instances = 0;

static void murmurhash3_x86_32_ref_nif_make_atoms(ErlNifEnv *env);
static int murmurhash3_x86_32_ref_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int murmurhash3_x86_32_ref_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data,
                                              ERL_NIF_TERM load_info);
static void murmurhash3_x86_32_ref_nif_unload(ErlNifEnv *env, void *priv_data);

static void
murmurhash3_x86_32_ref_nif_make_atoms(ErlNifEnv *env)
{
#define ATOM(Id, Value)                                                                                                \
    {                                                                                                                  \
        Id = enif_make_atom(env, Value);                                                                               \
    }
    ATOM(ATOM_badarg, "badarg");
    ATOM(ATOM_error, "error");
    ATOM(ATOM_false, "false");
    ATOM(ATOM_nil, "nil");
    ATOM(ATOM_notsup, "notsup");
    ATOM(ATOM_ok, "ok");
    ATOM(ATOM_true, "true");
    ATOM(ATOM_undefined, "undefined");
#undef ATOM
    return;
}

static int
murmurhash3_x86_32_ref_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Initialize private data. */
    (void)env;
    (void)priv_data;
    (void)load_info;

    /* Initialize common atoms. */
    (void)murmurhash3_x86_32_ref_nif_make_atoms(env);

    murmurhash3_x86_32_ref_nif_instances++;

    return retval;
}

static int
murmurhash3_x86_32_ref_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Upgrade private data. */
    (void)env;
    (void)new_priv_data;
    (void)old_priv_data;
    (void)load_info;

    murmurhash3_x86_32_ref_nif_instances++;

    return retval;
}

static void
murmurhash3_x86_32_ref_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)env;

    if (murmurhash3_x86_32_ref_nif_instances == 1) {
        /* Destroy private data. */
        (void)priv_data;
    }

    murmurhash3_x86_32_ref_nif_instances--;

    return;
}

ERL_NIF_INIT(murmurhash3_x86_32_ref_nif, murmurhash3_x86_32_ref_nif_funcs, murmurhash3_x86_32_ref_nif_load, NULL,
             murmurhash3_x86_32_ref_nif_upgrade, murmurhash3_x86_32_ref_nif_unload);

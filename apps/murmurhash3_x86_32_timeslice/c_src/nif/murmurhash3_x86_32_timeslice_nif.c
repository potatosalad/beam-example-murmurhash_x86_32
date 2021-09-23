#include "murmurhash3_x86_32_timeslice_nif.h"

#include <stdbool.h>
#include <unistd.h>

#include "murmurhash3_x86_32.h"

/* Static Variables */

static ERL_NIF_TERM ATOM_badarg;
static ERL_NIF_TERM ATOM_error;
static ERL_NIF_TERM ATOM_false;
static ERL_NIF_TERM ATOM_nil;
static ERL_NIF_TERM ATOM_notsup;
static ERL_NIF_TERM ATOM_ok;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_undefined;

static ErlNifResourceType *murmurhash3_x86_32_timeslice_nif_trap_resource_type = NULL;

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

/* Resource Type Functions (Declarations) */

typedef struct murmurhash3_x86_32_timeslice_nif_trap_s murmurhash3_x86_32_timeslice_nif_trap_t;

struct murmurhash3_x86_32_timeslice_nif_trap_s {
    ErlNifEnv *work_env;
    murmurhash3_x86_32_ctx_t *ctx;
    ErlNifBinary key_bin;
    size_t key_off;
    ErlNifBinary out_bin;
};

static void murmurhash3_x86_32_timeslice_nif_trap_dtor(ErlNifEnv *env, void *obj);

void
murmurhash3_x86_32_timeslice_nif_trap_dtor(ErlNifEnv *caller_env, void *obj)
{
    (void)caller_env;
    murmurhash3_x86_32_timeslice_nif_trap_t *trap = (void *)obj;
    if (trap != NULL && trap->ctx != NULL) {
        (void)murmurhash3_x86_32_destroy(trap->ctx);
        (void)enif_free((void *)trap->ctx);
        trap->ctx = NULL;
        (void)enif_release_binary(&trap->out_bin);
        (void)enif_free_env(trap->work_env);
    }
    return;
}

/* NIF Function Declarations */

static ERL_NIF_TERM murmurhash3_x86_32_timeslice_nif_hash_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM murmurhash3_x86_32_timeslice_nif_hash_2_continue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Function Definitions */

#define REDUCTIONS_UNTIL_YCF_YIELD() (10000)
#define BUMP_ALL_REDS(env)                                                                                             \
    do {                                                                                                               \
        (void)enif_consume_timeslice((env), 100);                                                                      \
    } while (0)
#define BUMP_REMAINING_REDS(env, nr_of_reductions)                                                                     \
    do {                                                                                                               \
        (void)enif_consume_timeslice(                                                                                  \
            (env), (int)((REDUCTIONS_UNTIL_YCF_YIELD() - (nr_of_reductions)) / REDUCTIONS_UNTIL_YCF_YIELD()));         \
    } while (0)

ERL_NIF_TERM
murmurhash3_x86_32_timeslice_nif_hash_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned long seed_ulong;
    ERL_NIF_TERM key_term;
    ErlNifBinary key_bin;
    ErlNifBinary out_bin;
    murmurhash3_x86_32_ctx_t *ctx = NULL;
    ErlNifEnv *work_env = NULL;
    long nr_of_reductions;
    bool is_yielded = false;
    size_t slice_len;

    // Allocate work environment, we will use this if we need to yield.
    work_env = enif_alloc_env();
    if (work_env == NULL) {
        return EXCP_ERROR(env, "Can't allocate work_env = enif_alloc_env()");
    }

    if (argc != 2) {
        (void)enif_free_env(work_env);
        return EXCP_NOTSUP(env, "argc must be 2");
    }
    if (!enif_get_ulong(env, argv[0], &seed_ulong) || seed_ulong > 0xFFFFFFFF) {
        (void)enif_free_env(work_env);
        return EXCP_BADARG(env, "Bad argument: 'Seed'");
    }
    if (!enif_is_binary(env, argv[1]) && !enif_is_list(env, argv[1])) {
        (void)enif_free_env(work_env);
        return EXCP_BADARG(env, "Bad argument: 'Key'");
    }

    // Copy the key binary to the work environment so it will be kept when we are yielding.
    key_term = enif_make_copy(work_env, argv[1]);
    if (!enif_inspect_iolist_as_binary(work_env, key_term, &key_bin)) {
        (void)enif_free_env(work_env);
        return EXCP_BADARG(env, "Bad argument: 'Key'");
    }

    // Allocate out binary
    if (!enif_alloc_binary(4, &out_bin)) {
        (void)enif_free_env(work_env);
        return EXCP_ERROR(env, "Can't allocate 'Out' binary");
    }

    // Allocate hash context
    ctx = (void *)enif_alloc(sizeof(murmurhash3_x86_32_ctx_t));
    if (ctx == NULL) {
        (void)enif_release_binary(&out_bin);
        (void)enif_free_env(work_env);
        return EXCP_ERROR(env, "Can't allocate murmurhash3_x86_32_ctx_t");
    }

    (void)murmurhash3_x86_32_init(ctx, (uint32_t)seed_ulong);

    nr_of_reductions = REDUCTIONS_UNTIL_YCF_YIELD();

    is_yielded = (nr_of_reductions < key_bin.size);
    slice_len = (is_yielded) ? nr_of_reductions : key_bin.size;
    (void)murmurhash3_x86_32_update(ctx, key_bin.data, slice_len);
    nr_of_reductions -= slice_len;

    if (is_yielded) {
        BUMP_ALL_REDS(env);
        murmurhash3_x86_32_timeslice_nif_trap_t *trap =
            enif_alloc_resource(murmurhash3_x86_32_timeslice_nif_trap_resource_type, sizeof(*trap));
        if (trap == NULL) {
            (void)murmurhash3_x86_32_destroy(ctx);
            (void)enif_free((void *)ctx);
            (void)enif_release_binary(&out_bin);
            (void)enif_free_env(work_env);
            return EXCP_ERROR(env, "Can't allocate murmurhash3_x86_32_timeslice_nif_trap_t");
        }
        trap->work_env = work_env;
        trap->ctx = ctx;
        trap->key_bin = key_bin;
        trap->key_off = slice_len;
        trap->out_bin = out_bin;
        ERL_NIF_TERM newargv[1];
        newargv[0] = enif_make_resource(env, (void *)trap);
        (void)enif_release_resource((void *)trap);
        return enif_schedule_nif(env, "murmurhash3_x86_32_timeslice_nif_hash_2_continue", ERL_NIF_NORMAL_JOB_BOUND,
                                 murmurhash3_x86_32_timeslice_nif_hash_2_continue, 1, newargv);
    } else {
        BUMP_REMAINING_REDS(env, nr_of_reductions);
        (void)murmurhash3_x86_32_final(ctx, out_bin.data);
        (void)murmurhash3_x86_32_destroy(ctx);
        (void)enif_free((void *)ctx);
        (void)enif_free_env(work_env);
        return enif_make_binary(env, &out_bin);
    }
}

ERL_NIF_TERM
murmurhash3_x86_32_timeslice_nif_hash_2_continue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    long nr_of_reductions = REDUCTIONS_UNTIL_YCF_YIELD();
    bool is_yielded = false;
    size_t slice_len;
    murmurhash3_x86_32_timeslice_nif_trap_t *trap = NULL;

    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_resource(env, argv[0], murmurhash3_x86_32_timeslice_nif_trap_resource_type, (void **)&trap)) {
        return EXCP_BADARG(env, "Bad argument: 'Trap'");
    }
    is_yielded = (nr_of_reductions < (trap->key_bin.size - trap->key_off));
    slice_len = (is_yielded) ? nr_of_reductions : (trap->key_bin.size - trap->key_off);
    (void)murmurhash3_x86_32_update(trap->ctx, trap->key_bin.data + trap->key_off, slice_len);
    trap->key_off += slice_len;
    nr_of_reductions -= slice_len;
    if (is_yielded) {
        BUMP_ALL_REDS(env);
        return enif_schedule_nif(env, "murmurhash3_x86_32_timeslice_nif_hash_2_continue", ERL_NIF_NORMAL_JOB_BOUND,
                                 murmurhash3_x86_32_timeslice_nif_hash_2_continue, argc, argv);
    } else {
        BUMP_REMAINING_REDS(env, nr_of_reductions);
        (void)murmurhash3_x86_32_final(trap->ctx, trap->out_bin.data);
        (void)murmurhash3_x86_32_destroy(trap->ctx);
        trap->ctx = NULL;
        (void)enif_free_env(trap->work_env);
        return enif_make_binary(env, &trap->out_bin);
    }
}

/* NIF Callbacks */

static ErlNifFunc murmurhash3_x86_32_timeslice_nif_funcs[] = {
    {"hash", 2, murmurhash3_x86_32_timeslice_nif_hash_2, ERL_NIF_NORMAL_JOB_BOUND},
};

static int murmurhash3_x86_32_timeslice_nif_instances = 0;

static void murmurhash3_x86_32_timeslice_nif_make_atoms(ErlNifEnv *env);
static int murmurhash3_x86_32_timeslice_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int murmurhash3_x86_32_timeslice_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data,
                                              ERL_NIF_TERM load_info);
static void murmurhash3_x86_32_timeslice_nif_unload(ErlNifEnv *env, void *priv_data);

static void
murmurhash3_x86_32_timeslice_nif_make_atoms(ErlNifEnv *env)
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
murmurhash3_x86_32_timeslice_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Initialize resource types. */
    murmurhash3_x86_32_timeslice_nif_trap_resource_type =
        enif_open_resource_type(env, "murmurhash3_x86_32_timeslice_nif", "murmurhash3_x86_32_timeslice_nif_trap",
                                murmurhash3_x86_32_timeslice_nif_trap_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (murmurhash3_x86_32_timeslice_nif_trap_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    /* Initialize private data. */
    (void)priv_data;
    (void)load_info;

    /* Initialize common atoms. */
    (void)murmurhash3_x86_32_timeslice_nif_make_atoms(env);

    murmurhash3_x86_32_timeslice_nif_instances++;

    return retval;
}

static int
murmurhash3_x86_32_timeslice_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Upgrade resource types. */
    murmurhash3_x86_32_timeslice_nif_trap_resource_type =
        enif_open_resource_type(env, "murmurhash3_x86_32_timeslice_nif", "murmurhash3_x86_32_timeslice_nif_trap",
                                murmurhash3_x86_32_timeslice_nif_trap_dtor, ERL_NIF_RT_TAKEOVER, NULL);
    if (murmurhash3_x86_32_timeslice_nif_trap_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    /* Upgrade private data. */
    (void)env;
    (void)new_priv_data;
    (void)old_priv_data;
    (void)load_info;

    murmurhash3_x86_32_timeslice_nif_instances++;

    return retval;
}

static void
murmurhash3_x86_32_timeslice_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)env;

    if (murmurhash3_x86_32_timeslice_nif_instances == 1) {
        /* Destroy private data. */
        (void)priv_data;
    }

    murmurhash3_x86_32_timeslice_nif_instances--;

    return;
}

ERL_NIF_INIT(murmurhash3_x86_32_timeslice_nif, murmurhash3_x86_32_timeslice_nif_funcs, murmurhash3_x86_32_timeslice_nif_load, NULL,
             murmurhash3_x86_32_timeslice_nif_upgrade, murmurhash3_x86_32_timeslice_nif_unload);

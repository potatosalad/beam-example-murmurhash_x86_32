#include "murmurhash3_x86_32_ycf_nif.h"

#include <stdbool.h>
#include <unistd.h>

#include "../codegen/murmurhash3_x86_32.h"
#include "murmurhash3_x86_32_ycf.h"

/* Static Variables */

static ERL_NIF_TERM ATOM_badarg;
static ERL_NIF_TERM ATOM_error;
static ERL_NIF_TERM ATOM_false;
static ERL_NIF_TERM ATOM_nil;
static ERL_NIF_TERM ATOM_notsup;
static ERL_NIF_TERM ATOM_ok;
static ERL_NIF_TERM ATOM_true;
static ERL_NIF_TERM ATOM_undefined;

static ErlNifResourceType *murmurhash3_x86_32_ycf_nif_trap_resource_type = NULL;

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

typedef struct murmurhash3_x86_32_ycf_nif_trap_s murmurhash3_x86_32_ycf_nif_trap_t;

struct murmurhash3_x86_32_ycf_nif_trap_s {
    void *state;
    ErlNifEnv *work_env;
    murmurhash3_x86_32_ctx_t *ctx;
    ErlNifBinary out_bin;
};

static void *murmurhash3_x86_32_ycf_nif_trap_allocator(size_t size, void *context);
static void murmurhash3_x86_32_ycf_nif_trap_freer(void *data, void *context);
static void murmurhash3_x86_32_ycf_nif_trap_dtor(ErlNifEnv *env, void *obj);

void *
murmurhash3_x86_32_ycf_nif_trap_allocator(size_t size, void *context)
{
    (void)context;
    return enif_alloc(size);
}

void
murmurhash3_x86_32_ycf_nif_trap_freer(void *data, void *context)
{
    (void)context;
    (void)enif_free(data);
}

void
murmurhash3_x86_32_ycf_nif_trap_dtor(ErlNifEnv *caller_env, void *obj)
{
    (void)caller_env;
    murmurhash3_x86_32_ycf_nif_trap_t *trap = (void *)obj;
    if (trap != NULL && trap->state != NULL) {
        (void)murmurhash3_x86_32_update_ycf_gen_destroy(trap->state);
        if (trap->ctx != NULL) {
            (void)murmurhash3_x86_32_destroy(trap->ctx);
            (void)enif_free((void *)trap->ctx);
            trap->ctx = NULL;
        }
        (void)enif_release_binary(&trap->out_bin);
        (void)enif_free_env(trap->work_env);
        trap->state = NULL;
    }
    return;
}

/* NIF Function Declarations */

static ERL_NIF_TERM murmurhash3_x86_32_ycf_nif_hash_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM murmurhash3_x86_32_ycf_nif_hash_2_continue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* NIF Function Definitions */

#define REDUCTIONS_UNTIL_YCF_YIELD() (4000)

ERL_NIF_TERM
murmurhash3_x86_32_ycf_nif_hash_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned long seed_ulong;
    ERL_NIF_TERM key_term;
    ErlNifBinary key_bin;
    ErlNifBinary out_bin;
    murmurhash3_x86_32_ctx_t *ctx = NULL;
    ErlNifEnv *work_env = NULL;
    long nr_of_reductions;
    void *trap_state = NULL;

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

    // Copy the key binary to the work environemnt so it will be kept when we are yielding.
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

    (void)murmurhash3_x86_32_update_ycf_gen_yielding(
        &nr_of_reductions,                         // long *ycf_nr_of_reductions_param
        &trap_state,                               // void **ycf_trap_state
        NULL,                                      // void *ycf_extra_context
        murmurhash3_x86_32_ycf_nif_trap_allocator, // ycf_yield_alloc_type ycf_yield_alloc
        murmurhash3_x86_32_ycf_nif_trap_freer,     // ycf_yield_free_type ycf_yield_free
        NULL,                                      // void *ycf_yield_alloc_free_context
        64,                                        // size_t ycf_stack_alloc_size_or_max_size
        NULL,                                      // void *ycf_stack_alloc_data
        ctx,                                       // murmurhash3_x86_32_ctx_t *ctx_N2_N6
        key_bin.data,                              // uint8_t *key_N3_N7
        key_bin.size                               // size_t key_len_N4_N8
    );

    if (YCF_IS_YIELDED(trap_state)) {
        murmurhash3_x86_32_ycf_nif_trap_t *trap =
            enif_alloc_resource(murmurhash3_x86_32_ycf_nif_trap_resource_type, sizeof(*trap));
        if (trap == NULL) {
            (void)murmurhash3_x86_32_destroy(ctx);
            (void)enif_free((void *)ctx);
            (void)enif_release_binary(&out_bin);
            (void)enif_free_env(work_env);
            return EXCP_ERROR(env, "Can't allocate murmurhash3_x86_32_ycf_nif_trap_t");
        }
        trap->state = trap_state;
        trap->work_env = work_env;
        trap->ctx = ctx;
        trap->out_bin = out_bin;
        ERL_NIF_TERM newargv[1];
        newargv[0] = enif_make_resource(env, (void *)trap);
        (void)enif_release_resource((void *)trap);
        return enif_schedule_nif(env, "murmurhash3_x86_32_ycf_nif_hash_2_continue", ERL_NIF_NORMAL_JOB_BOUND,
                                 murmurhash3_x86_32_ycf_nif_hash_2_continue, 1, newargv);
    } else {
        (void)murmurhash3_x86_32_final(ctx, out_bin.data);
        (void)murmurhash3_x86_32_destroy(ctx);
        (void)enif_free((void *)ctx);
        (void)enif_free_env(work_env);
        return enif_make_binary(env, &out_bin);
    }
}

ERL_NIF_TERM
murmurhash3_x86_32_ycf_nif_hash_2_continue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    long nr_of_reductions = REDUCTIONS_UNTIL_YCF_YIELD();
    murmurhash3_x86_32_ycf_nif_trap_t *trap = NULL;
    if (argc != 1) {
        return EXCP_NOTSUP(env, "argc must be 1");
    }
    if (!enif_get_resource(env, argv[0], murmurhash3_x86_32_ycf_nif_trap_resource_type, (void **)&trap)) {
        return EXCP_BADARG(env, "Bad argument: 'Trap'");
    }
    (void)murmurhash3_x86_32_update_ycf_gen_continue(&nr_of_reductions, // long *ycf_number_of_reduction_param
                                                     &trap->state,      // void **ycf_trap_state
                                                     NULL               // void *ycf_extra_context
    );
    if (YCF_IS_YIELDED(trap->state)) {
        return enif_schedule_nif(env, "murmurhash3_x86_32_ycf_nif_hash_2_continue", ERL_NIF_NORMAL_JOB_BOUND,
                                 murmurhash3_x86_32_ycf_nif_hash_2_continue, argc, argv);
    } else {
        (void)murmurhash3_x86_32_final(trap->ctx, trap->out_bin.data);
        (void)murmurhash3_x86_32_destroy(trap->ctx);
        trap->ctx = NULL;
        (void)enif_free_env(trap->work_env);
        return enif_make_binary(env, &trap->out_bin);
    }
}

/* NIF Callbacks */

static ErlNifFunc murmurhash3_x86_32_ycf_nif_funcs[] = {
    {"hash", 2, murmurhash3_x86_32_ycf_nif_hash_2, ERL_NIF_NORMAL_JOB_BOUND},
};

static int murmurhash3_x86_32_ycf_nif_instances = 0;

static void murmurhash3_x86_32_ycf_nif_make_atoms(ErlNifEnv *env);
static int murmurhash3_x86_32_ycf_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int murmurhash3_x86_32_ycf_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data,
                                              ERL_NIF_TERM load_info);
static void murmurhash3_x86_32_ycf_nif_unload(ErlNifEnv *env, void *priv_data);

static void
murmurhash3_x86_32_ycf_nif_make_atoms(ErlNifEnv *env)
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
murmurhash3_x86_32_ycf_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Initialize resource types. */
    murmurhash3_x86_32_ycf_nif_trap_resource_type =
        enif_open_resource_type(env, "murmurhash3_x86_32_ycf_nif", "murmurhash3_x86_32_ycf_nif_trap",
                                murmurhash3_x86_32_ycf_nif_trap_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (murmurhash3_x86_32_ycf_nif_trap_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    /* Initialize private data. */
    (void)priv_data;
    (void)load_info;

    /* Initialize common atoms. */
    (void)murmurhash3_x86_32_ycf_nif_make_atoms(env);

    murmurhash3_x86_32_ycf_nif_instances++;

    return retval;
}

static int
murmurhash3_x86_32_ycf_nif_upgrade(ErlNifEnv *env, void **new_priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    int retval = 0;

    /* Upgrade resource types. */
    murmurhash3_x86_32_ycf_nif_trap_resource_type =
        enif_open_resource_type(env, "murmurhash3_x86_32_ycf_nif", "murmurhash3_x86_32_ycf_nif_trap",
                                murmurhash3_x86_32_ycf_nif_trap_dtor, ERL_NIF_RT_TAKEOVER, NULL);
    if (murmurhash3_x86_32_ycf_nif_trap_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    /* Upgrade private data. */
    (void)env;
    (void)new_priv_data;
    (void)old_priv_data;
    (void)load_info;

    murmurhash3_x86_32_ycf_nif_instances++;

    return retval;
}

static void
murmurhash3_x86_32_ycf_nif_unload(ErlNifEnv *env, void *priv_data)
{
    (void)env;

    if (murmurhash3_x86_32_ycf_nif_instances == 1) {
        /* Destroy private data. */
        (void)priv_data;
    }

    murmurhash3_x86_32_ycf_nif_instances--;

    return;
}

ERL_NIF_INIT(murmurhash3_x86_32_ycf_nif, murmurhash3_x86_32_ycf_nif_funcs, murmurhash3_x86_32_ycf_nif_load, NULL,
             murmurhash3_x86_32_ycf_nif_upgrade, murmurhash3_x86_32_ycf_nif_unload);

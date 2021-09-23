%% % @format
-module(murmurhash3_x86_32_ref_nif).

-on_load(init/0).

%% NIF API
-export([
    hash/2,
    hash_dirty/2
]).

%% Types
-type seed() :: 0..16#FFFFFFFF.
-type out() :: <<_:32>>.

-export_type([
    seed/0,
    out/0
]).

%%%=============================================================================
%%% NIF API functions
%%%=============================================================================

-spec hash(Seed, Key) -> Out when Seed :: seed(), Key :: iolist(), Out :: out().
hash(_Seed, _Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec hash_dirty(Seed, Key) -> Out when Seed :: seed(), Key :: iolist(), Out :: out().
hash_dirty(_Seed, _Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
init() ->
    SoName = filename:join(murmurhash3_x86_32_ref:priv_dir(), ?MODULE_STRING),
    erlang:load_nif(SoName, 0).

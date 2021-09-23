%% % @format
-module(murmurhash3_x86_32_ref).

%% API
-export([
    reds/0,
    reds_dirty/0,
    hash/2
]).
%% Internal API
-export([
    priv_dir/0
]).

%% Macros
-define(is_seed(Seed), (is_integer(Seed) andalso Seed >= 0 andalso Seed =< 16#FFFFFFFF)).
-define(is_key(Key), (is_binary(Key) orelse is_list(Key))).

%% Types
-type seed() :: 0..16#FFFFFFFF | <<_:0>> | <<_:8>> | <<_:16>> | <<_:24>> | <<_:32>>.
-type key() :: iodata().
-export_type([
    seed/0,
    key/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

reds() ->
    Key = binary:copy(<<255>>, 1 * 1024 * 1024),
    {reductions, R1} = erlang:process_info(self(), reductions),
    Out = hash(0, Key),
    {reductions, R2} = erlang:process_info(self(), reductions),
    {Out, R2 - R1}.

reds_dirty() ->
    Key = binary:copy(<<255>>, 1 * 1024 * 1024),
    {reductions, R1} = erlang:process_info(self(), reductions),
    Out = murmurhash3_x86_32_ref_nif:hash_dirty(0, Key),
    {reductions, R2} = erlang:process_info(self(), reductions),
    {Out, R2 - R1}.

-spec hash(Seed, Key) -> Out when Seed :: seed(), Key :: iolist(), Out :: murmurhash3_x86_32_ref_nif:out().
hash(Seed, Key) when ?is_seed(Seed) andalso ?is_key(Key) ->
    murmurhash3_x86_32_ref_nif:hash(Seed, Key);
hash(Seed, Key) when is_binary(Seed) andalso byte_size(Seed) =< 4 andalso ?is_key(Key) ->
    hash(binary:decode_unsigned(Seed, little), Key).

%%%=============================================================================
%%% Internal API functions
%%%=============================================================================

-spec priv_dir() -> file:filename_all().
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

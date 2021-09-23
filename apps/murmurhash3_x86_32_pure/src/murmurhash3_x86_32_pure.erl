%% % @format
-module(murmurhash3_x86_32_pure).

%% API
-export([
    reds/0,
    hash/2,
    init/1,
    update/2,
    final/1
]).

%% Records
-record(murmurhash3_x86_32_pure, {
    h1 = 0 :: 0..16#FFFFFFFF,
    buffer = <<>> :: binary(),
    length = 0 :: 0..16#FFFFFFFF
}).

%% Types
-type ctx() :: #murmurhash3_x86_32_pure{}.
-type seed() :: 0..16#FFFFFFFF.
-type out() :: <<_:32>>.

-export_type([
    ctx/0,
    seed/0,
    out/0
]).

%% Macros
-define(is_seed(Seed), (is_integer(Seed) andalso Seed >= 0 andalso Seed =< 16#FFFFFFFF)).
-define(is_key(Key), (is_binary(Key) orelse is_list(Key))).
-define(MASK32(X), ((X) band 16#FFFFFFFF)).
-define(MULT32(X, Y), ?MASK32((X) * (Y))).
-define(ROTL32(X, R), ?MASK32(((X) bsl (R)) bor ((X) bsr (32 - (R))))).

%%%=============================================================================
%%% API functions
%%%=============================================================================

reds() ->
    Key = binary:copy(<<255>>, 1 * 1024 * 1024),
    {reductions, R1} = erlang:process_info(self(), reductions),
    Out = hash(0, Key),
    {reductions, R2} = erlang:process_info(self(), reductions),
    {Out, R2 - R1}.

-spec hash(Seed, Key) -> Out when Seed :: seed(), Key :: iolist(), Out :: out().
hash(Seed, Key) when ?is_seed(Seed) andalso ?is_key(Key) ->
    Ctx0 = init(Seed),
    Ctx1 = update(Ctx0, Key),
    final(Ctx1).

-spec init(Seed) -> NewCtx when Seed :: seed(), NewCtx :: ctx().
init(Seed) when ?is_seed(Seed) ->
    #murmurhash3_x86_32_pure{h1 = Seed};
init(Seed) when is_binary(Seed) andalso byte_size(Seed) =< 4 ->
    init(binary:decode_unsigned(Seed, little)).

-spec update(OldCtx, Key) -> NewCtx when OldCtx :: ctx(), Key :: iolist(), NewCtx :: ctx().
update(Ctx = #murmurhash3_x86_32_pure{}, <<>>) ->
    Ctx;
update(_Ctx0 = #murmurhash3_x86_32_pure{h1 = H1, buffer = Buf, length = Len}, Key) when
    byte_size(Buf) > 0 andalso byte_size(Buf) + byte_size(Key) >= 4
->
    x86_32_update_body(H1, Len, <<Buf/binary, Key/binary>>);
update(_Ctx0 = #murmurhash3_x86_32_pure{h1 = H1, buffer = <<>>, length = Len}, Key) when
    byte_size(Key) >= 4
->
    x86_32_update_body(H1, Len, Key);
update(Ctx0 = #murmurhash3_x86_32_pure{buffer = Buf}, Key) when byte_size(Buf) + byte_size(Key) < 4 ->
    Ctx1 = Ctx0#murmurhash3_x86_32_pure{buffer = <<Buf/binary, Key/binary>>},
    Ctx1;
update(Ctx = #murmurhash3_x86_32_pure{}, Key) when is_list(Key) ->
    update(Ctx, erlang:iolist_to_binary(Key)).

-spec final(OldCtx) -> Out when OldCtx :: ctx(), Out :: out().
final(#murmurhash3_x86_32_pure{h1 = H1_0, buffer = <<>>, length = Len}) ->
    H1_1 = ?MASK32(H1_0 bxor Len),
    H1_2 = fmix32(H1_1),
    <<H1_2:1/little-unsigned-integer-unit:32>>;
final(Ctx0 = #murmurhash3_x86_32_pure{h1 = H1_0, buffer = Buf, length = Len0}) when byte_size(Buf) > 0 ->
    {{H1_1}, Len1} = x86_32_tail({H1_0}, {0}, Buf, Len0),
    Ctx1 = Ctx0#murmurhash3_x86_32_pure{h1 = H1_1, buffer = <<>>, length = Len1},
    final(Ctx1).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
fmix32(H_0) ->
    H_1 = ?MASK32(H_0 bxor (H_0 bsr 16)),
    H_2 = ?MULT32(H_1, 16#85ebca6b),
    H_3 = ?MASK32(H_2 bxor (H_2 bsr 13)),
    H_4 = ?MULT32(H_3, 16#c2b2ae35),
    H_5 = ?MASK32(H_4 bxor (H_4 bsr 16)),
    H_5.

%% @private
x86_32_update_body(H1_0, Len0, <<K1_0:1/little-unsigned-integer-unit:32, Key/binary>>) ->
    C1 = 16#cc9e2d51,
    C2 = 16#1b873593,
    K1_1 = ?MULT32(K1_0, C1),
    K1_2 = ?ROTL32(K1_1, 15),
    K1_3 = ?MULT32(K1_2, C2),
    H1_1 = ?MASK32(H1_0 bxor K1_3),
    H1_2 = ?ROTL32(H1_1, 13),
    H1_3 = ?MASK32(?MULT32(H1_2, 5) + 16#e6546b64),
    Len1 = Len0 + 4,
    x86_32_update_body(H1_3, Len1, Key);
x86_32_update_body(H1, Len, Buffer) ->
    #murmurhash3_x86_32_pure{h1 = H1, buffer = Buffer, length = Len}.

%% @private
x86_32_tail({H1}, {K1}, <<>>, Len) ->
    {{?MASK32(H1 bxor K1)}, Len};
x86_32_tail(H, {K1_0}, Buf0, Len) ->
    C1 = 16#cc9e2d51,
    C2 = 16#1b873593,
    BLen = byte_size(Buf0),
    <<Buf1:(BLen - 1)/binary, Tail>> = Buf0,
    case BLen of
        3 ->
            K1_1 = ?MASK32(K1_0 bxor (Tail bsl 16)),
            x86_32_tail(H, {K1_1}, Buf1, Len + 1);
        2 ->
            K1_1 = ?MASK32(K1_0 bxor (Tail bsl 8)),
            x86_32_tail(H, {K1_1}, Buf1, Len + 1);
        1 ->
            K1_1 = ?MASK32(K1_0 bxor Tail),
            K1_2 = ?MULT32(K1_1, C1),
            K1_3 = ?ROTL32(K1_2, 15),
            K1_4 = ?MULT32(K1_3, C2),
            x86_32_tail(H, {K1_4}, Buf1, Len + 1)
    end.

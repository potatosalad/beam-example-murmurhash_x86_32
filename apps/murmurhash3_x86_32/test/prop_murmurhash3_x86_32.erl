%% % @format
-module(prop_murmurhash3_x86_32).
-include_lib("proper/include/proper.hrl").

-export([
    prop_ref/0,
    prop_timeslice/0,
    prop_ycf/0
]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_ref() ->
    ?FORALL(
        {Seed, Key},
        {seed(), key()},
        begin
            Expected = murmurhash3_x86_32_pure:hash(Seed, Key),
            Challenge = murmurhash3_x86_32_ref:hash(Seed, Key),
            Expected =:= Challenge
        end
    ).

prop_timeslice() ->
    ?FORALL(
        {Seed, Key},
        {seed(), key()},
        begin
            Expected = murmurhash3_x86_32_pure:hash(Seed, Key),
            Challenge = murmurhash3_x86_32_timeslice:hash(Seed, Key),
            Expected =:= Challenge
        end
    ).

prop_ycf() ->
    ?FORALL(
        {Seed, Key},
        {seed(), key()},
        begin
            Expected = murmurhash3_x86_32_pure:hash(Seed, Key),
            Challenge = murmurhash3_x86_32_ycf:hash(Seed, Key),
            Expected =:= Challenge
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

seed() -> integer(0, 16#FFFFFFFF).

key() -> union([key_binary(), key_iolist()]).

key_binary() -> binary().

key_iolist() -> ?LET(Len, range(0, 4), vector(Len, binary())).

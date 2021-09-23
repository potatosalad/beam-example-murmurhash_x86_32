%%%-------------------------------------------------------------------
%% @doc murmurhash3_x86_32 public API
%% @end
%%%-------------------------------------------------------------------

-module(murmurhash3_x86_32_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    murmurhash3_x86_32_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

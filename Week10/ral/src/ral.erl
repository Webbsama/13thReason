%%%-------------------------------------------------------------------
%% @doc ral public API
%% @end
%%%-------------------------------------------------------------------

-module(ral).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ral_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

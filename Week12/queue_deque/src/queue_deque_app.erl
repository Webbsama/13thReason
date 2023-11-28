%%%-------------------------------------------------------------------
%% @doc queue_deque public API
%% @end
%%%-------------------------------------------------------------------

-module(queue_deque_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    queue_deque_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

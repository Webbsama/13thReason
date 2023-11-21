%%%-------------------------------------------------------------------
%% @doc trie public API
%% @end
%%%-------------------------------------------------------------------

-module(trie_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    trie_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

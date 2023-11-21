-module(trie).

-export([]).

%% Type definitions go here
% trie_node
-type trie_node() :: definition.

%% Function specs and functions go here
% lookup()
-spec lookup([term()], trie_node()) -> found|found_fail.
lookup(Target, Trie) ->
    todo.

% add()
-spec add([term()], trie_node()) -> trie_node().
add(Item, Trie) ->
    todo.

%% Testing segment begins here
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% EUnit function tests go here

-endif.
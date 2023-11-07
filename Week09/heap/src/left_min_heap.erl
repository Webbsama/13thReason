-module(left_min_heap).

-export([]).

-type heap_node() :: {integer(), term(), heap_node()|nil, heap_node()|nil}.
-type min_heap() :: heap_node().

-spec get_min(min_heap()) -> term().
get_min(Heap) ->
    todo.

% Test time BBy
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_min_test() ->
    [
        ?_assertEqual(Expect, Expr)
    ].

-endif.
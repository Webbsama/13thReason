-module(left_min_heap).

-export([]).

-type heap_node() :: {integer(), term(), heap_node()|nil, heap_node()|nil}.
-type min_heap() :: heap_node().

-spec get_min(min_heap()) -> term().
get_min(Heap) ->
    todo.


-spec insert(term(), heap_node()) -> min_heap().
insert(Value, Heap) ->
    todo.


-spec merge(min_heap(), min_heap()) -> min_heap().
merge(A_heap, B_heap) ->
    todo.


-spec build_node(term(), min_heap(), min_heap()) -> min_heap().
build_node(X, A_heap, B_heap) ->
    todo.


-spec rank(heap_node()) -> integer().
rank(A_heap) ->
    todo.


-spec remove_min(min_heap()) -> term().
remove_min(Heap) ->
    todo.


% Test time BBy
% 1. Understand the "In -> Out"
% 2. Write the Happy Path Tests
% 3. Code it
% 4. Write the Nasty Thoughts Tests
% 5. Code more
% 6. Repeat steps 4 and 5 multiple times
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_min_test() ->
    [
        ?_assertEqual(Expect, Expr)
    ].

-endif.
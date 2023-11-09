-module(left_min_heap).

-export([get_min/1, insert/2, merge/2, remove_min/1, build_node/3]).

-type heap_node() :: {pos_integer(), term(), heap_node()|nil, heap_node()|nil}.
-type min_heap() :: heap_node() | nil.

-spec is_empty(min_heap()) -> true|false.
is_empty(nil) -> true;
is_empty(_Heap) -> false.


% The specification for get_min
-spec get_min(min_heap()) -> term().
% Handle if nil gets passed in
get_min(nil) -> nil;
% Now what to do the proper stuff is passed in
get_min({_rank, Value, _next_l, _next_r}) -> Value.

-spec insert(term(), heap_node()) -> min_heap().
% Handle if nil is passed in
insert(nil, nil) -> nil;
% Handle if a proper heap is passed in
insert(Value, Heap) ->
    merge({1, Value, nil, nil}, Heap).


-spec merge(min_heap(), min_heap()) -> min_heap().
% Handle if nil heap is passed in for either A_heap or B_heap
merge(nil, B_heap) -> B_heap;
merge(A_heap, nil) -> A_heap;
% Now how to merge our two heaps
merge({_A_rank, A_value, Al_sub, Ar_sub}, {_B_rank, B_value, _Bl_sub, _Br_sub} = B_heap) when A_value =< B_value ->
    build_node(A_value, Al_sub, (merge(Ar_sub, B_heap)));
merge({_A_rank, _A_value, _Al_sub, _Ar_sub} = A_heap, {_B_rank, B_value, Bl_sub, Br_sub}) ->
    build_node(B_value, Bl_sub, (merge(A_heap, Br_sub))).


% Returns a node with a value of X, 
% a rank one greater than the lesser rank of A_heap or B_heap,
% with A_heap and B_heap as children, with the greater ranked node on the left.
-spec build_node(term(), min_heap(), min_heap()) -> min_heap().
build_node(X, A_heap, B_heap) ->
    case
        rank(A_heap) >= rank(B_heap) of 
        true -> {(rank(B_heap)+1), X, A_heap, B_heap};
        false -> {(rank(A_heap)+1), X, B_heap, A_heap}
    end.


-spec rank(heap_node()) -> integer().
% Handling if nil is passsed in.
rank(nil) -> 0;
% Establish what the rank is.
rank({Rank, _Value, _Left, _Right}) -> Rank.


-spec remove_min(min_heap()) -> min_heap().
%remove_min(nil) -> nil;
remove_min({_Rank, _Value, Left, Right}) ->
    merge(Left, Right).

% Test time BBy
% 1. Understand the "In -> Out"
% 2. Write the Happy Path Tests
% 3. Code it
% 4. Write the Nasty Thoughts Tests
% 5. Code more
% 6. Repeat steps 4 and 5 multiple times

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% 
% # Leftist Min-Heap Implementation

% # Node structure with rank, value, left subtree, and right subtree
% Node = {rank, value, left_subtree, right_subtree}

% # Function to get the minimum value from the heap
% get_min :: node -> Integer or nil
% get_min :: nil -> nil;
% get_min :: {rank, value, left, right} -> value.
get_min_test_() ->
    [
        % Happy thoughts! (you can fly)
        ?_assertEqual(3, get_min({2, 3, {1, 4, nil, nil}, {1, 5, nil, nil}})),
        ?_assertEqual(nil, get_min(nil))
    ].

% # Function to insert a value into the heap
% insert :: value node -> heap or nil
% insert :: nil nil -> nil;
% insert :: value heap -> merge :: {1, value, nil, nil} heap.
insert_test_() -> 
    [
        % Happy thoughts! (you can fly)
        ?_assertEqual({1, 3, {1, 4, nil, nil}, nil}, insert(4, {1, 3, nil, nil})),
        %Neutral Thoughts
        ?_assertEqual(nil, insert(nil, nil))
    ].

% # Function to merge two heaps
% - The first set of parameters `{rank_h, value_h, left_h, right_h}` represents the parameters of the first heap being merged.
% merge :: heap heap -> heap
% merge :: nil heap -> heap;
% merge :: heap nil -> heap;
% merge :: {rank_h, value_h, left_h, right_h} {rank_i, value_i, left_i, right_i} 
% -> build_node(value_h, left_h, merge(right_h, {rank_i, value_i, left_i, right_i}))
%   where value_h <= value_i
% 
% - The second set `{rank_i, value_i, left_i, right_i}` represents the parameters of the second heap being merged.
%   otherwise build_node(value_i, left_i, merge({rank_h, value_h, left_h, right_h}, right_i)).
merge_test_() ->
    % Happy thoughts! (you can fly)
    [
        ?_assertEqual({1, 3, {1, 4, nil, nil}, nil}, merge({1,4, nil, nil}, {1, 3, nil, nil})),
        %Neutral Thoughts
        ?_assertEqual({1, 3, nil, nil}, merge(nil, {1, 3, nil, nil})),
        ?_assertEqual({1, 3, nil, nil}, merge({1, 3, nil, nil}, nil))
    ].

%  The goal is to create a new node with the given value `x`
% and subtrees `a` and `b`, updating the rank accordingly

% # Sub-algorithm to build a node with updated rank
% - `x` represents the value of the current node being built.
% - `a` represents the left subtree.
% - `b` represents the right subtree.
% build_node :: x a b -> {rank, x, a, b}
% build_node :: x a b where rank a >= rank b -> {rank b + 1, x, a, b};
% build_node :: x a b -> {rank a + 1, x, b, a}.
build_node_test_() ->
    % Happy thoughts! (you can fly)
    [   
        ?_assertEqual({2, 3, {1, 4, nil, nil}, {1, 5, nil, nil}}, build_node(3, {1, 4, nil, nil}, {1, 5, nil, nil}))
    ].

% # Sub-algorithm to determine the rank of a node
% rank :: node -> Integer
% rank :: nil -> 0
% rank :: {rank, value, left, right} -> rank.
    rank_test_() ->
    % Happy thoughts! (you can fly)
    [
        ?_assertEqual(2, rank({2, 3, {1, 4, nil, nil}, {1, 5, nil, nil}})),
        ?_assertEqual(0, rank(nil))
    ].

% # Function to remove the minimum element from the heap
% remove_min :: heap -> heap or nil
% remove_min :: nil -> nil;
% remove_min :: {rank, value, left, right} -> merge(left, right).
remove_min_test_() ->
    % Happy thoughts! (you can fly)
    [
        ?_assertEqual({1, 4, {1, 5, nil, nil}, nil}, remove_min({2, 3, {1, 4, nil, nil}, {1, 5, nil, nil}}))
    ].

    -endif.
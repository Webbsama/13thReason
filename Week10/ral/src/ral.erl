%%%-------------------------------------------------------------------
%% @doc ral public API
%% @end
%%%-------------------------------------------------------------------

-module(ral).

% Need to fix our export once functions have been created. 
-export([]).

% Our first main function will be the function called get.
% Get has three helper functions.
% 1) search
% 2) build_bin_list
% 3) find

% tree :: {Count(parent) | Value(child), Left|nil, Right|nil}
-type tree() :: {pos_integer() | term(), tree()|nil, tree()|nil}.
-type ral() :: [tree()].

%% get(RAL, Index) -> Item at Index
-spec get(ral(), non_neg_integer()) -> term().
get(RAL, Index) ->
    todo.

% Get's first helper function called search
% search() retrieves the correct tree in the RAL for the given index.
% Looking for the tree with the list index and return the tree number and the tree 
%% search(RAL, Index, Skipped_indices) -> {skipped_indices, tree} | {fail, nil}
-spec search(ral(), non_neg_integer(), non_neg_integer()) -> {non_neg_integer(), tree()} | {fail, nil}.
search(RAL, Index, Accum) ->
    todo.

% Get's secound helper function called build_bin_list 
%% A traversal list is just the binary representation of the local index
-type traversal_list() :: [1|0]. 
%% build_bin_list(Local index, Binary space)
-spec build_bin_list(integer(), integer()) -> traversal_list().
build_bin_list(N, Bit_space) ->
    todo.

% Get's third helper function called find
% Find has two parameters the 
-spec find(tree(), traversal_list()) -> integer().
find(Tree, Traversal_list) ->
    todo.

% Our second main function will be called update It changes a value
% Update has one helper function called replace
%% update(RAL, Index, Value, Accum) -> RAL
-spec update(ral(),non_neg_integer(), term(), non_neg_integer()) -> ral().
update(RAL, Index, Value, Accum) ->
    todo.

% Replace is a helper function to update
-spec replace(tree(), traversal_list()) -> tree().
replace(Tree, Traversal_list) ->
    todo.

% Our third main function will be called cons
-spec cons(tree(), ral()) -> ral().
cons(Leaf, RAL) ->
    todo.

% Our forth (and final) main function will be called link
-spec link(tree(), ral()) -> todo.
link(Tree, RAL) ->
    todo.

% Will need to add our eunit tests.
% There will be 5 eunit tests. This is one for each of our main functions
% Remember that the test's names need to end with an underscore so that the tests compile correctly. 
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% First test will be for get function
% 


% Input -> Output
% get(Test_RAL, 5) -> fifth
get_test_() ->
    {setup,
    Test_RAL = fun() ->
        [
            {zeroth, nil, nil},
            {2, {first, nil, nil}, {second, nil, nil}},
            {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}}
        ]
    end,

    [
        ?assertEqual(nil, get([], 3)),
        ?assertEqual(zeroth, get(Test_RAL, 0)),
        ?assertEqual(second, get(Test_RAL, 2)),
        ?assertEqual(fifth, get(Test_RAL, 5))
        % nasty thoughts
    ]}.

search_test_() ->
    {setup,
    fun () ->
        Test_RAL = [
            {zeroth, nil, nil},
            {2, {first, nil, nil}, {second, nil, nil}},
            {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}}
        ]
    end,

    Tree_at_index_two = {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}},
    [
        ?assertEqual({fail, nil}, search([], 1, 0)),
        ?assertEqual({fail, nil}, search(Test_RAL, 100, 0)),
        ?assertEqual({3, Tree_at_index_two}, search(Test_RAL, 5, 0))
    ]}.

build_bin_list_test_() ->
    [
        ?assertEqual([1], build_bin_list(1, 2)),
        ?assertEqual([1,0], build_bin_list(2, 4)),
        ?assertEqual([0,1,1], build_bin_list(3, 8)),
        ?assertEqual([1,1,1], build_bin_list(7, 8)),
        ?assertEqual([1,0,0,0,1], build_bin_list(17, 32))
    ].


find_test_() ->
    Test_tree_1 = {2, {first, nil, nil}, {second, nil, nil}},
    Test_tree_2 = {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}},
    [
        ?assertEqual(second, find(Test_tree_1, [1])),
        ?assertEqual(fourth, find(Test_tree_2, [0, 1])),
        ?assertEqual(sixth, find(Test_tree_2, [1, 1]))
    ].
% Second test will be for update function
% update_test_() ->
%     [
%         ?assertEqual(Expect, update(RAL, Index, Value, Accum))
%     ].

% % Third test will be for replace function 
% replace_test_() ->
%     [
%         ?assertEqual(Expect, Expr)
%     ].

% % Fourth test will be for cons function
% cons_test_() ->
%     [
%         ?assertEqual(Expect, Expr)
%     ].

% % Fifth test will be for link function
% link_test_() ->
%     [
%         ?assertEqual(Expect, Expr)
%     ].

-endif.
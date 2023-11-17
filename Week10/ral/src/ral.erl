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
%% build_bin_list(
-spec build_bin_list(integer(), integer()) -> [1|0].
build_bin_list(N, Bit_space) ->
    todo.

% Get's third helper function called find
% Find has two parameters the 
-spec find(tree(), [1|0]) -> integer().
find(Tree, Transversal_list) ->
    todo.

% Our second main function will be called update It changes a value
% Update has one helper function called replace
%% update(RAL, Index, Value, Accum) -> RAL
-spec update(ral(),non_neg_integer(), term(), non_neg_integer()) -> ral().
update(RAL, Index, Value, Accum) ->
    todo.

% Replace is a helper function to update
-spec replace() -> todo.
replace() ->
    todo.

% Our third main function will be called cons
-spec cons() -> todo. 
cons() ->
    todo.

% Our forth (and final) main function will be called link
-spec link() -> todo. 
link() ->
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
    Test_RAL = [
        {zeroth, nil, nil},
        {2 {first, nil, nil}, {second, nil, nil}},
        {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}}
    ],
    [
        ?assertEqual(nil, get([], 3)),
        ?assertEqual(zeroth, get(Test_RAL, 0)),
        ?assertEqual(second, get(Test_RAL, 2)),
        ?assertEqual(fifth, get(Test_RAL, 5)),
        % nasty thoughts
    ].

search_test_() ->
    Test_RAL = [
        {zeroth, nil, nil},
        {2 {first, nil, nil}, {second, nil, nil}},
        {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}}
    ],
    Tree_at_index_two = {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}},
    [
        ?assertEqual({fail, nil}, search([], 1, 0)),
        ?assertEqual({fail, nil}, search(Test_RAL, 100, 0)),
        %% IF THIS BREAKS, WE PROBS NEED TO USE THE INDEX OF THE TREE, RATHER THAN N AS IN NTH TREE
        ?assertEqual({3, Tree_at_index_two}, search(Test_RAL, 5, 0))
    ].

build_bin_list_test_() ->
    [
        ?assertEqual(Expect, Expr)
        ?assertEqual(Expect, Expr)
    ].


find_test_() ->
    [
        ?assertEqual(Expect, Expr)
    ].
% Second test will be for update function
update_test_() ->
    [
        ?assertEqual(Expect, Expr)
    ].

% Third test will be for replace function 
replace_test_() ->
    [
        ?assertEqual(Expect, Expr)
    ].

% Fourth test will be for cons function
cons_test_() ->
    [
        ?assertEqual(Expect, Expr)
    ].

% Fifth test will be for link function
link_test_() ->
    [
        ?assertEqual(Expect, Expr)
    ].

-endif.
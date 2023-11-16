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
%% search(RAL, Index, Skipped_indices) -> {fail|skipped_indices, tree|nil}
-spec search(ral(), non_neg_integer(), non_neg_integer()) -> {non_neg_integer(), tree()} | {fail, nil}.
search(RAL, Index, Accum) ->
    todo.

% Get's secound helper function called build_bin_list
%% build_bin_list(
-spec build_bin_list(integer(), integer()) -> bitstring().
build_bin_list(N, Bit_space) ->
    todo.

% Get's thrid helper function called find
% Find has two parameters the 
-spec find(tree(), bitstring()) -> integer(). 
find(Tree, Transversal_list) ->
    todo.

% Our second main function will be called update It changes a value
% Update has one helper function called replace
-spec update(ral(),non_neg_integer()) -> ral().
update() ->
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
Test_RAL = [
    {zeroth, nil, nil},
    {2 {first, nil, nil}, {second, nil, nil}},
    {4, {2, {third, nil, nil}, {fourth, nil, nil}}, {2, {fifth, nil, nil}, {sixth, nil, nil}}}
].

% Input -> Output
% get(Test_RAL, 5) -> fifth
get_test_() ->
    [
        ?assertEqual(nil, get([], 3)),
        ?assertEqual(zeroth, get(Test_RAL, 0)),
        ?assertEqual(second, get(Test_RAL, 2)),
        ?assertEqual(fifth, get(Test_RAL, 5)),
        % nasty thoughts
    ].

search_test_() ->
    [
        ?assertEqual(Expect, Expr)
        ?assertEqual(Expect, Expr)
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
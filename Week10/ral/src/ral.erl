%%%-------------------------------------------------------------------
%% @doc ral public API
%% @end
%%%-------------------------------------------------------------------

-module(ral).

% Need to fix our export once functions have been created. 
-export([]).

% Added a type for our ral()
-type ral() :: []

% Our first main function will be the function called get.
% Get has three helper functions.
% 1) search
% 2) build_bin_list
% 3) find
% The get function takes two parameters
-spec get(List, Integer) -> Value.

% Get's first helper function called search
-spec search() -> todo.

% Get's secound helper function called build_bin_list
-spec build_bin_list() -> todo.

% Get's thrid helper function called find
% Find has two parameters the 
-spec find([]) -> value. 

% Our second main function will be called update
-spec update() -> todo.

% Our third main function will be called replace
-spec replace() -> todo. 

% Our fourth main function will be called cons
-spec cons() -> todo. 

% Our fifth (and final) main function will be called link
-spec link() -> todo. 

% Will need to add our eunit tests.
% There will be 5 eunit tests. This is one for each of our main functions
% Remember that the test's names need to end with an underscore so that the tests compile correctly. 
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% First test will be for get function
get_test_() ->
    [

    ].

% Second test will be for update function
update_test_() ->
    [

    ].

% Third test will be for replace function 
replace_test_() ->
    [

    ].

% Fourth test will be for cons function
cons_test_() ->
    [

    ].

% Fifth test will be for link function
link_test_() ->
    [

    ].

-endif.
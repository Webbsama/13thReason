-module(trie).

-export([lookup/2, add/2]).

%% Type definitions go here
% trie_node
-type trie_node() :: {char(), [trie_node()|nil]}.

%% Function specs and functions go here
% lookup()
-spec lookup([term()], trie_node()) -> found|not_found.


lookup([], Trie) ->
    {_Value, Children} = Trie,
    case lists:member(nil, Children) of
        true ->
            found;
        _ ->
            not_found
    end;
lookup(Target, Trie) ->
    [FirstLetter|Tail] = Target,
    {_Value, Children} = Trie,
    Subtree = find_in_list(FirstLetter, Children),
    case Subtree of
        not_found ->
            fail;
        _ ->
            lookup(Tail, Subtree)
        end.
    
% If an element in the list matches the patter, returns the element, otherwise return fail.
find_in_list(_Target, []) -> not_found;
find_in_list(Target, List = [{Value, _} = Child|T]) ->
    case Value == Target of
        true -> 
            Child;
        _ -> 
            find_in_list(Target, T)
    end.
% add()
-spec add([term()], trie_node()) -> trie_node().
add(Item, Trie) ->
    todo.

%% Testing segment begins here
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% EUnit function tests go here
lookup_test_() ->
    [
        % Happy Path
        ?assertEqual(found, lookup("c", { "c"[Hello, nil]}))
        ?assertEqual(found, lookup("cat", {"c", [{"a",[{"r", [nil]}, {"t", [nil]}]}]})),
        ?assertEqual(found, lookup("car", {"c", [{"a",[{"r", [nil]}, {"t", [nil]}]}]})),
        ?assertEqual(not_found, lookup("dog", {"c", [{"a",[{"r", [nil]}, {"t", [nil]}]}]}))
    ].

add_test_() ->
    [
        % Happy Path
        ?assertEqual({"c", [{"a",[{"r", [nil]}, {"t", [nil]}]}]}, add("car", {"c", [{"a",[{"t", [nil]}]}]}))
    ].

-endif.
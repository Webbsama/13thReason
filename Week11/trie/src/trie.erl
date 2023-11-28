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
    Subtrie = search_children(FirstLetter, Children),
    case Subtrie of
        not_found ->
            not_found;
        _ ->
            lookup(Tail, Subtrie)
        end.
    
% If an element in the list matches the patter, returns the element, otherwise return fail.
-spec search_children(term(), [trie_node()|nil]) -> trie_node()|nil.
search_children(_Target, []) -> not_found;
search_children(Target, [nil|T]) -> search_children(Target, T);
search_children(Target, [{Value, Children}|T]) ->
    case Value == Target of
        true -> 
            {Value, Children};
        _ -> 
            search_children(Target, T)
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
        ?assertEqual(found, lookup("c", { "c", [nil]})),
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
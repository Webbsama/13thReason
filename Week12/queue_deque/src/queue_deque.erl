-module(queue_deque).

-export([flip/1 ,empty/1, head/1, tail/1, enqueue/2, dequeue/1, enqueue_front/2, dequeue_back/1]).

% Rest of stuff here. 
-type queue() :: {[term()], [term()]}.
% Each time an element is added to the queue, prepend to R (Rear).
% Each time element is removed from queue, remove from F (Front).
% Note that when Front is empty Rear is reversed and Rear becomes the front

% Helper function to flip our queue around
-spec flip(list()) -> list().
flip([]) -> [];
flip([Item]) ->
    [Item];
flip(List) ->
    [Head | Tail] = List,
    flip(Tail)++Head.

% A queue (tuple) of F (Front) and R (Rear).
-spec empty(queue()) -> boolean().
empty({[], []}) ->
    true;
empty(_) ->
    false.

% Returns what the current Head of the queue 
-spec head(queue()) -> term().
head({[],[]}) ->
    nil;
head({[], Rear}) ->
    [Head|_Tail] = flip(Rear),
    Head;
head({Front, []}) ->
    [Head|_Tail] = Front,
    Head.

% Returns the the current Tail of the queue
-spec tail(queue()) -> term().
tail({[], Rear}) ->
    [Head, _Tail] = 
    Head.

% Adds an element to the back of the queue
-spec enqueue(queue(), term()) -> queue().
enqueue(Queue, Term) -> 
    todo.

% Returns an element from the front of the queue's waited his turn
% Returns the updated queue F and R
-spec dequeue(queue()) -> term().
dequeue(Queue) -> 
    todo.

% Adds an element to the front of the queue
-spec enqueue_front(queue(), term()) -> queue().
enqueue_front(Queue, Term) -> 
    todo.
% Returns an element from the back of the queue
% (He's given up on waiting)enqueue_front(queue, term) ->
-spec dequeue_back(queue()) -> term().
dequeue_back(Queue) -> 
    todo.

% Tests go here
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
flip_test() ->
    [
        ?_assertEqual(["d", "c", "b", "a"], flip(["a", "b", "c", "d"])),
        ?_assertEqual(["c", "b", "a"], flip(["a", "b", "c"])),
        ?_assertEqual(["a"], flip(["a"])),
        ?_assertEqual([], flip([]))
    ].
empty_test_() ->
    [
        ?_assertEqual(true, empty({[], []})),
        ?_assertEqual(false, empty({["a"], ["b"]}))
    ].

head_test_() ->
    [
        ?_assertEqual(nil, head({[], []})),
        ?_assertEqual("a", head({["a"], []})),
        ?_assertEqual("a", head({[], ["c", "b", "a"]}))
        ?_assertEqual( , head({[a, b, c], f[e, d]}))
    ].

tail_test_() -> 
    [
        ?_assertEqual(nil, tail({[], []})),
        ?_assertEqual("a", tail({["a"], []})),
        ?_assertEqual("c", tail({[], ["c", "b", "a"]})),
        ?_assertEqual("a", tail({["c", "b", "a"], []}))
    ].

enqueue_test_() ->
    [
        ?_assertEqual({["a"], []}, enqueue({[], []}, "a")),
        ?_assertEqual({["a", "b"], ["c"]}, enqueue({["a", "b"], []}, "c")),
        ?_assertEqual({["a", "b"], ["d","c"]}, enqueue({["a", "b"], ["c"]}, "d")),
        ?_assertEqual({["a", "b"], ["e","d","c"]}, enqueue({["a", "b"], ["d","c"]}, "e"))
        % Ideas for other tests for enqueue?
    ].

dequeue_test_() ->
    [
        ?_assertEqual(nil, dequeue({[],[]})),
        ?_assertEqual({[], []}, dequeue({["a"], []})),
        ?_assertEqual({["b"], ["c"]}, dequeue({["a", "b"], ["c"]})),
        ?_assertEqual({["b"], ["d","c"]}, dequeue({["a", "b"], ["d","c"]})),
        ?_assertEqual({[], ["e","d","c"]}, dequeue({["b"], ["e","d","c"]})),
        ?_assertEqual({["d", "e"], []}, dequeue({[], ["e","d","c"]}))
    ].

enqueue_front_test_() ->
    [
        ?_assertEqual({["a"], []}, enqueue_front({[], []}, "a")),
        ?_assertEqual({["c", "a", "b"], []}, enqueue_front({["a", "b"], []}, "c")),
        ?_assertEqual({["c", "a", "b"], ["d"]}, enqueue_front({["a", "b"], ["c"]}, "d")),
        ?_assertEqual({["f", "a", "b"], ["e","d","c"]}, enqueue_front({["a","b"], ["e","d","c"]}, "f"))
    ].
    
dequeue_back_test_() ->
    [
        ?_assertEqual({[], []}, dequeue_back({["a"], []})),
        ?_assertEqual({["a", "b"], []}, dequeue_back({["a", "b"], ["c"]})),
        ?_assertEqual({["a", "b"], ["c"]}, dequeue_back({["a", "b"], ["d","c"]})),
        ?_assertEqual({["b"], ["d","c"]}, dequeue_back({["b"], ["e","d","c"]})),
        ?_assertEqual({[], ["d", "e"]}, dequeue_back({["e","d","c"], []})),
        ?_assertEqual({[], ["d","c"]}, dequeue_back({[], ["e","d","c"]}))
    ].

-endif.
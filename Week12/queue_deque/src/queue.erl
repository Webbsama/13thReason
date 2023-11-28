-module(queue).

-export([empty/1, head/1, tail/1, enqueue/2, dequeue/1, enqueue_front/2, dequeue_back/1]).

% Rest of stuff here. 
-type queue() :: {[term()], [term()]}.
% Each time an element is added to the queue, prepend to R (Rear).
% Each time element is removed from queue, remove from F (Front).
% Note that when Front is empty Rear is reversed and Rear becomes the front

% A queue (tuple) of F (Front) and R (Rear).
-spec empty(queue()) -> boolean().
empty(queue) ->
    todo. 

% Returns what the current Head of the queue 
-spec head(queue()) -> term().
head(queue) ->
    todo.

% Returns the the current Tail of the queue
-spec tail(queue()) -> term().
tail(queue) ->
    todo. 

% Adds an element to the back of the queue
-spec enqueue(queue(), term()) -> queue().
enqueue(queue, term) -> 
    todo.

% Returns an element from the front of the queue's waited his turn
% Returns the updated queue F and R
-spec dequeue(queue()) -> term().
dequeue(queue) -> 
    todo.

% Adds an element to the front of the queue
-spec enqueue_front(queue(), term()) -> queue().
enqueue_front(queue, term) -> 
    todo.
% Returns an element from the back of the queue
% (He's given up on waiting)enqueue_front(queue, term) ->
-spec dequeue_back(queue()) -> term().
dequeue_back(queue) -> 
    todo.

% Tests go here
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_test_() ->
    [
        ?assert(true, empty({[], []})),
        ?assert(false, empty({["a"], ["b"]}))
    ].

head_test_() ->
    [
        ?assert(nil, head({[], []})),
        ?assert("a", head({["a"], []})),
        ?assert("a", head({[], ["c", "b", "a"]}))
    ].

tail_test_() -> 
    [
        ?assert(nil, tail({[], []})),
        ?assert("a", tail({["a"], []})),
        ?asssert("c", tail({[], ["c", "b", "a"]})),
        ?assert("a", tail(["c", "b", "a"], []))
    ].

enqueue_test_() ->
    [
        ?assert({["a"], []}, enqueue({[], []}, "a")),
        ?assert({["a", "b"], ["c"]}, enqueue({["a", "b"], []}, "c")),
        ?assert({["a", "b"], ["d","c"]}, enqueue({["a", "b"], ["c"]}, "d")),
        ?assert({["a", "b"], ["e","d","c"]}, enqueue({["a", "b"], ["d","c"]}, "e"))
        % Ideas for other tests for enqueue?
    ].

dequeue_test_()->
    [
        ?assert(nil, dequeue({[],[]}))
        ?assert({[], []}, dequeue({["a"], []})),
        ?assert({["b"], ["c"]}, dequeue({["a", "b"], ["c"]})),
        ?assert({["b"], ["d","c"]}, dequeue({["a", "b"], ["d","c"]})),
        ?assert({[], ["e","d","c"]}, dequeue({["b"], ["e","d","c"]})),
        ?assert({["d", "e"], []}, dequeue({[], ["e","d","c"]}))
    ].

enqueue_front_test_() ->
    [
        ?assert({["a"], []}, enqueue_front({[], []}, "a")),
        ?assert({["c", "a", "b"], []}, enqueue_front({["a", "b"], []}, "c")),
        ?assert({["c", "a", "b"], ["d"]}, enqueue_front({["a", "b"], ["c"]}, "d")),
        ?assert({["f", "a", "b"], ["e","d","c"]}, enqueue_front({["a","b"], ["e","d","c"]}, "f"))
    ].
    
dequeue_back_test_() ->
    [
        ?assert({[], []}, dequeue_back({["a"], []})),
        ?assert({["a", "b"], []}, dequeue_back({["a", "b"], ["c"]})),
        ?assert({["a", "b"], ["c"]}, dequeue_back({["a", "b"], ["d","c"]})),
        ?assert({["b"], ["d","c"]}, dequeue_back({["b"], ["e","d","c"]})),
        ?assert({}, dequeue_back({["e","d","c"], []})),
        ?assert({[], ["d","c"]}, dequeue_back({[], ["e","d","c"]}))
    ].

-endif.
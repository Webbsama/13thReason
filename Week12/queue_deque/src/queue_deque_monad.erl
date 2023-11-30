-module(queue_deque_monad).

-export([count/1 ,flip/1 ,empty/1, head/1, tail/1, enqueue/2, dequeue/1, enqueue_front/2, dequeue_back/1]).

% Rest of stuff here. 
-type queue() :: {[term()], [term()]}.

% queue_monad :: {Queue, Count}
-type queue_monad() :: {queue(), non_neg_integer()}.

% Each time an element is added to the queue, prepend to R (Rear).
% Each time element is removed from queue, remove from F (Front).
% Note that when Front is empty Rear is reversed and Rear becomes the front

% Returns the count!
% 
-spec count(queue_monad()) -> non_neg_integer().
count({_Queue, Count}) -> Count.



% Helper function to flip our queue around
-spec flip(list()) -> list().
flip([]) -> [];
flip([Item]) ->
    [Item];
flip(List) ->
    [Head | Tail] = List,
    flip(Tail)++[Head].

% A queue (tuple) of F (Front) and R (Rear).
-spec empty(queue_monad()) -> boolean().
empty({{[], []}, _Count}) ->
    true;
empty(_) ->
    false.

% Returns what the current Head of the queue
-spec head(queue_monad()) -> term().
head({{[],[]}, _Count}) ->
    nil;
head({{[], Rear}, _Count}) ->
    [Head|_Tail] = flip(Rear),
    Head;
head({{Front, _Rear}, _Count}) ->
    [Head|_Tail] = Front,
    Head.

% Returns the the current Tail of the queue
-spec tail(queue_monad()) -> term().
tail({{[], []}, _Count}) -> nil;
tail({{[], Rear}, _Count}) ->
    [Head|_Tail] = Rear,
    Head;
tail({{Front, []}, _Count}) ->
    [Head|_Tail] = flip(Front),
    Head.

% Adds an element to the back of the queue
-spec enqueue(queue_monad(), term()) -> queue_monad().
enqueue({{[], []}, _Count}, Term) ->
    {{[Term], []}, 1};
enqueue({{Front, Rear}, Count}, Term) -> 
    {{Front, [Term] ++ Rear}, Count+1}. 

% Returns an element from the front of the queue, (He's waited his turn)
% Returns the updated queue F and R
-spec dequeue(queue_monad()) -> queue_monad().
dequeue({{[], []}}) -> {[], []};
dequeue({{[], Rear}}) ->
    dequeue({flip(Rear), []});
dequeue({{[_H|[]], Rear}}) ->
    {{[], Rear}};
dequeue({[_H|T], Rear}) ->
    {{T, Rear}}.


% Adds an element to the front of the queue
-spec enqueue_front(queue_monad(), term()) -> queue_monad().
enqueue_front({{Front, Rear}, Count}, Term) -> 
    {{[Term] ++ Front, Rear}, Count}.


% Returns an element from the back of the queue (He's given up on waiting)
% enqueue_front(queue, term) ->
-spec dequeue_back(queue_monad()) -> queue_monad().
dequeue_back({{[], []}, Count}) ->
    {{[], []}, Count};
dequeue_back({{Front, []}, Count}) ->
    [_H|T] = flip(Front),
    {{[], T}, Count};
dequeue_back({{F, [_H|T]}, Count}) -> 
    {{F,T}, Count}.

% Tests go here
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

flip_test() ->
    [
        ?_assertEqual([d, c, b, a], flip([a, b, c, d])),
        ?_assertEqual([c, b, a], flip([a, b, c])),
        ?_assertEqual([a], flip([a])),
        ?_assertEqual([], flip([]))
    ].
empty_test_() ->
    [
        ?_assertEqual(true, empty({{[], []}, 0})),
        ?_assertEqual(false, empty({{[a], [b]}, 2}))
    ].

count_test_() ->
    [
        ?_assertEqual(0, count({{[], []}, 0})),
        ?_assertEqual(1, count({{[a], []}, 1})),
        ?_assertEqual(3, count({{[], [c, b, a]}, 3})),
        ?_assertEqual(5, count({{[a, b, c], [e, d]}, 5}))
    ].

head_test_() ->
    [
        ?_assertEqual(nil, head({{[], []}, 0})),
        ?_assertEqual(a, head({{[a], []}, 1})),
        ?_assertEqual(a, head({{[], [c, b, a]}, 3})),
        ?_assertEqual(a, head({{[a, b, c], [e, d]}, 5}))
    ].

tail_test_() -> 
    [
        ?_assertEqual(nil, tail({{[], []}, 0})),
        ?_assertEqual(a, tail({{[a], []}, 1})),
        ?_assertEqual(c, tail({{[], [c, b, a]}, 3})),
        ?_assertEqual(a, tail({{[c, b, a], []}, 3}))
    ].

enqueue_test_() ->
    [
        ?_assertEqual({{[a], []}, 1}, enqueue({{[], []}, 0}, a)),
        ?_assertEqual({{[a, b], [c]}, 3}, enqueue({{[a, b], []}, 2}, c)),
        ?_assertEqual({{[a, b], [d,c]}, 4}, enqueue({{[a, b], [c]}, 3}, d)),
        ?_assertEqual({{[a, b], [e,d,c]}, 5}, enqueue({{[a, b], [d,c]}, 4}, e))
        % Ideas for other tests for enqueue?
    ].

dequeue_test_() ->
    [
        % I think that we should use atoms rather than strings due to Erlang's String/List ambiguity.
        ?_assertEqual({{[], []}, 0}, dequeue({{[],[]}, 0})),
        ?_assertEqual({{[], []}, 0}, dequeue({{[a], []}, 1})),
        ?_assertEqual({{[b], [c]}, 2}, dequeue({{[a, b], [c]}, 3})),
        ?_assertEqual({{[b], [d,c]}, 3}, dequeue({{[a, b], [d,c]}, 4})),
        ?_assertEqual({{[], [e,d,c]}, 3}, dequeue({{[b], [e,d,c]}, 4})),
        ?_assertEqual({{[d, e], []}, 2}, dequeue({{[], [e,d,c]}, 3}))
    ].

enqueue_front_test_() ->
    [
        ?_assertEqual({{[a], []}, 1}, enqueue_front({{[], []}, 0}, a)),
        ?_assertEqual({{[c, a, b], []}, 3}, enqueue_front({{[a, b], []}, 2}, c)),
        ?_assertEqual({{[d, a, b], [c]}, 4}, enqueue_front({{[a, b], [c]}, 3}, d)), % This test feels sus, we should revisit him
        ?_assertEqual({{[f, a, b], [e,d,c]}, 6}, enqueue_front({{[a,b], [e,d,c]}, 5}, f))
    ].
    
dequeue_back_test_() ->
    [
        ?_assertEqual({{[], []}, 0}, dequeue_back({{[a], []}, 1})),
        ?_assertEqual({{[a, b], []}, 2}, dequeue_back({{[a, b], [c]}, 3})),
        ?_assertEqual({{[a, b], [c]}, 3}, dequeue_back({{[a, b], [d, c]}, 4})),
        ?_assertEqual({{[b], [d,c]}, 3}, dequeue_back({{[b], [e,d,c]}, 4})),
        ?_assertEqual({{[], [d, e]}, 2}, dequeue_back({{[e,d,c], []}, 3})),
        ?_assertEqual({{[], [d,c]}, 2}, dequeue_back({{[], [e,d,c]}, 3}))
    ].

-endif.
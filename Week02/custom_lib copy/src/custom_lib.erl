-module(custom_lib).

-export([prepending/2, remove/1]).

% writing a function

prepending(Thing, List) ->
    {Thing, List}.

remove({Head, Tail}) ->
    Tail.

%when running you will go into the shell and then write custom_lib:prepending(a,{}).
% this will now return {a,{}}.
% List1 = custom_lib:prepending(a, {}).
% This will now return {a,{}}
% List2 - custom_lib:prepending(64, List1).
% {64, {a,{}}}
% List3 = custom_lib:prepending(mario, List2).
% {mario,{64,{a,{}}}}
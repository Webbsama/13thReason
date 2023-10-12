
%%%-------------------------------------------------------------------
%%% @author Lee Barney
%%% @copyright 2023 Lee Barney licensed under the <a>
%%%        rel="license"
%%%        href="http://creativecommons.org/licenses/by/4.0/"
%%%        target="_blank">
%%%        Creative Commons Attribution 4.0 International License</a>
%%%
%%% Project - CSE 382
%%%
%%% @doc
%%% These solutions are not intended to be ideal solutions. Instead,
%%% they are solutions that you can compare against yours to see
%%% other options and to come up with even better solutions.
%%%
%%% You can find the unit tests to be passed near the bottom of this 
%%% file, just before the speed tests.
%%% @end
%%% Created : 8 May 2023 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------

-module(tasks).

%%%===================================================================
%%% Public API functions
%%%===================================================================
-export([c_map/2,c_filter/2,pick_candidates/1, filter(List, Predicate)).


%%%-------------------------------------------------------------------
%%% @doc
%%% Generates a list based on an initial list and a predicate to apply 
%%% to each element of the list.
%%%
%%% Parameters - 1)List of elements of any kind
%%%              2)Predicate used to modify each list element
%%% Value - a list of modified values
%%% Complexity - O(n)
%%% @end
%%%-------------------------------------------------------------------
%
% List comprehension !!!
c_map(List, Predicate) when is_list(List) ->
    lists:map(Predicate, List);
c_map(_, _) ->
    no_list_error.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% Input Validation:
%
%The code first checks if the input List is a valid list. If it's not a list, it returns an error.
%Main Mapping Operation:
%
%The function takes two arguments: List (the input list) and Predicate (a function to transform elements).
%It goes through each element in List.
%For each element, it applies the Predicate function and includes the result in the new list.
%Result:
%
%The result is a new list containing the transformed values of the elements from the input 
% List based on the Predicate function.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------------------------------------------------------------------
%%% @doc
%%% Generates a list based on an initial list and a predicate to apply 
%%% to each element of the list. The predicate returns true if the item
%%% is to be included in the resultant list. If not, the predicate 
%%% returns false.
%%%
%%% Parameters - 1)List of elements of any kind
%%%              2)Predicate used to evaluate the inclusion of each list element
%%% Value - a list of elements for which the predicate had a value of true
%%% Complexity - O(n)
%%% @end
%%%-------------------------------------------------------------------
filter(List, Predicate) when is_list(List) ->
    lists:filter(Predicate, List);
filter(_, _) ->
    no_list_error.
filter(List,Predicate)->
	[X||X<-List, Predicate(X)==true].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% Input Validation:
%
%The code first checks if the input List is a valid list. If it's not a list, it returns an error.
%Main Filtering Operation:
%
%The function takes two arguments: List (the input list) and Predicate (a condition or function).
%It goes through each element in List.
%If the Predicate is true for an element, that element is included in the result list.
%Result:
%
%The result is a new list containing only the elements from the input List that satisfy the 
%given condition (Predicate).
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


%%%-------------------------------------------------------------------
%%% @doc
%%% From a list of job applicants, this function selects those with Erlang 
%%% or JavaScript experience and assigns them either to be backend (Erlang
%%% or Erlang and JavaScript) or frontend (JavaScript only) job candidates. 
%%%
%%% Ordering of the candidates is preserved.
%%%
%%% Parameters - 1)List of applicants. Each applicant is a 3-tuple described 
%%%  				by {name, years_experience,[skill]} where [skill] is the 
%%%					candidates list of skills.
%%% Value - A list of candidates. Each candidate is a 4-tuple described
%%%					by {name, assigned_to,years_experience,[skill]} where
%%%					assigned_to can be either frontend or backend.
%%% Complexity - O(n)
%%% @end
%%%-------------------------------------------------------------------
pick_candidates(Applicants) ->
    lists:map(fun(Applicant) -> assign_candidate(Applicant) end, Applicants).

assign_candidate({Name, YearsExperience, Skills}) ->
    case lists:member(erlang, Skills) orelse lists:member(javascript, Skills) of
        true ->
            case lists:member(erlang, Skills) of
                true ->
                    {Name, backend, YearsExperience, Skills};
                false ->
                    {Name, frontend, YearsExperience, Skills}
            end;
        false ->
            % No Erlang or JavaScript skills, not a candidate
            {Name, not_a_candidate, YearsExperience, Skills}
    end.
% 
%pick_candidates(Applicants) when is_list(Applicants) ->
%    lists:map(fun assign_candidate/1, Applicants);
%pick_candidates(_) ->
%    no_list_error.

%assign_candidate({Name, YearsExperience, Skills}) ->
%    case lists:member(erlang, Skills) orelse lists:member(javascript, Skills) of
%        true ->
%            case lists:member(erlang, Skills) of
%                true ->
%                    {Name, backend, YearsExperience, Skills};
%                false ->
%                    {Name, frontend, YearsExperience, Skills}
%            end;
%        false ->
%            {Name, not_a_candidate, YearsExperience, Skills}
%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
%The provided Erlang code is a program that processes a list of job applicants and assigns them 
%to either frontend or backend positions based on their skills in Erlang and JavaScript. 
% Here's how it works:
%
%1. The `pick_candidates/1` function is the entry point for the program.
%  It takes a single argument, `Applicants`, which is a list of applicant records. 
% Each applicant record is represented as a 3-tuple with the following elements: `{Name, YearsExperience, Skills}`.
%
%2. The `lists:map` function is used to apply a function to each applicant in the `Applicants` list. 
% In this case, the function being applied is an anonymous function defined with `fun(Applicant) -> assign_candidate(Applicant) end`. It maps each applicant to the result of calling the `assign_candidate/1` function on that applicant.
%
%3. The `assign_candidate/1` function takes an individual applicant record as its argument.
%  Inside this function, the applicant's information is pattern-matched into three variables:
%  `Name`, `YearsExperience`, and `Skills`.
%
%4. The `case` expression is used to make decisions based on the applicant's skills. 
% It first checks if the applicant has either Erlang or JavaScript skills by using 
% the `lists:member/2` function to check if "erlang" or "javascript" is a member of the `Skills` list.
%
%5. If the applicant has either Erlang or JavaScript skills (the `true` branch of the outer `case`),
%  it checks again using another `case` expression to determine if the applicant has Erlang skills. 
% If they have Erlang skills (the `true` branch of the inner `case`), they are assigned to the "backend" position. Otherwise, they are assigned to the "frontend" position.
%
%6. If the applicant does not have Erlang or JavaScript skills (the `false` branch of the outer `case`), 
% they are marked as "not_a_candidate."
%
%7. The result of the `assign_candidate/1` function is a 4-tuple representing the applicant with the following elements:
%  `{Name, assigned_to, YearsExperience, Skills}`. The `assigned_to` element can take one of three values: 
% "backend," "frontend," or "not_a_candidate."
%
%8. Finally, the `pick_candidates/1` function returns a list of these 4-tuples, where each applicant has been assigned
%  to either frontend or backend positions based on their skills.
%
%In summary, this code processes a list of job applicants, checks their skills, and assigns them to
%  either frontend or backend positions accordingly. It then returns a list of the assigned candidates 
% while preserving the order of the original applicants.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

c_map_test_()->
	[?_assertEqual([2,4,6,10],c_map([1,2,3,5],fun(X)-> X*2 end)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],c_map([],fun(X)->X end)),
	 ?_assertEqual(no_list_error,c_map(nil,fun(X)-> X end)),
	 ?_assertEqual(no_list_error,c_map({hello,world},fun(_X)-> true end))
	].
c_filter_test_()->
	[?_assertEqual([2,4],c_filter([1,2,3,4,5],fun(X)-> (X rem 2) == 0 end)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],c_filter([],fun(_X)-> true end)),
	 ?_assertEqual(no_list_error,c_filter(nil,fun(_X)-> true end)),
	 ?_assertEqual(no_list_error,c_filter({hello,world},fun(_X)-> true end))
	].
pick_candidates_test_()->
	[?_assertEqual([{sue,2,backend,[erlang,c,rust]}],
					 pick_candidates([{joe,1,[c,r,swift]},
									  {sue,2,[erlang,c,rust]},
									  {sally,1,[kotlin]}])),%happy path
	 ?_assertEqual([{joe,1,frontend,[zap,javascript]}],
	 				pick_candidates([{joe,1,[zap,javascript]},
									  {sue,2,[c,rust]},
									  {sally,1,[kotlin]}])),%happy path
	 %nasty thoughts start here
	 ?_assertEqual([],pick_candidates([{joe,1,[zap,cobol]},
									  {sue,2,[c,rust]},
									  {sally,1,[kotlin]}])),
	 ?_assertEqual([],pick_candidates([])),
	 ?_assertEqual(no_list_error,pick_candidates(hello)),
	 ?_assertEqual(no_list_error,pick_candidates({hello,world}))
	].

-endif.



	
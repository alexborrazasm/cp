-module(stack).

-export([empty/0, push/2, pop/1, peek/1]).

% Creates an empty stack
empty() ->
    [].

% Adds an element to the top of the stack
push(Stack, Elem) ->
  [Elem | Stack].

% Removes the top element from the stack
pop(Stack) ->
    case Stack of
        [] ->
            [];
        [ _ | T ] ->
            T
    end.

% Examines the top element of the stack without removing it
peek(Stack) ->
    case Stack of
        [] ->
            empty;
        [ H | _ ] ->
            {ok, H}
    end.

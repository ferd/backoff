-module(prop_backoff_statem).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0]).
-export([command/1]).
-export([precondition/2]).
-export([next_state/3]).
-export([postcondition/3]).

-record(state, {backoff, type=normal, delay, start, max}).

init_args() ->
    ?SUCHTHAT([X,Y], [pos_integer(), oneof([pos_integer(), infinity])],
              X < Y).
type() ->
    elements([normal, jitter]).

initial_state() ->
    #state{}.

command(#state{backoff=undefined}) ->
    {call, backoff, init, init_args()};
command(#state{backoff=B}) ->
    oneof([{call, backoff, type, [B, type()]},
           {call, backoff, fail, [B]},
           {call, backoff, succeed, [B]},
           {call, backoff, get, [B]}]).

precondition(#state{backoff=B}, {call, _, init, _}) ->
    B =:= undefined;
precondition(#state{backoff=B}, _) ->
    B =/= undefined.

next_state(State, B, {call, _, init, [Start, Max]}) ->
    State#state{backoff=B, delay= Start, start=Start, max=Max};
next_state(#state{start=Start} = State, Value, {call, _, succeed, _}) ->
    NewB = {call, erlang, element, [2, Value]},
    State#state{backoff=NewB, delay=Start};
next_state(State, Value, {call, _, fail, _}) ->
    NewDelay = {call, erlang, element, [1, Value]},
    NewB = {call, erlang, element, [2, Value]},
    State#state{backoff=NewB, delay=NewDelay};
next_state(State, NewB, {call, _, type, [_, Type]}) ->
    State#state{backoff=NewB, type=Type};
next_state(State, _, {call, _, get, _}) ->
    State.

postcondition(#state{start=Start}, {call, _, succeed, _}, {NewDelay, _}) ->
    NewDelay =:= Start;
postcondition(#state{type=normal, delay=Delay, max=Max}, {call, _, fail, _},
              {NewDelay, _}) ->
    (NewDelay > Delay andalso NewDelay =< Max) orelse
    (NewDelay =:= Delay andalso NewDelay =:= Max);
postcondition(#state{type=jitter, delay=Delay, max=Max}, {call, _, fail, _},
              {NewDelay, _}) ->
    (NewDelay >= Delay orelse
     (Delay > Max div 3 andalso NewDelay >= 1 andalso NewDelay >= Max div 3))
    andalso NewDelay =< Max andalso Delay * 3 >= NewDelay;
postcondition(#state{delay=Delay}, {call, _, get, _}, Result) ->
    Result =:= Delay;
postcondition(_, _, _) ->
    true.

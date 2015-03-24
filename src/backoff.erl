-module(backoff).
-export([increment/1, increment/2]).
-export([rand_increment/1, rand_increment/2]).
-export([type/2]).
-export([init/2, init/4,
         fire/1, get/1, succeed/1, fail/1]).

-record(backoff, {start :: pos_integer(),
                  max :: pos_integer() | infinity,
                  current :: pos_integer(),
                  type=normal :: normal | jitter | decorrelated_jitter,
                  value :: term(),
                  dest :: pid()}).

-opaque backoff() :: #backoff{}.
-type type() :: normal | jitter | decorrelated_jitter.

-export_type([backoff/0, type/0]).

%% Just do the increments by hand!
-spec increment(pos_integer()) -> pos_integer().
increment(N) when is_integer(N) -> N bsl 1.

-spec increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
increment(N, Max) -> min(increment(N), Max).

%% Just do the random increments by hand!
%% Algorithm inspired in the Google HTTP Java client implementation of Class ExponentialBackOff.
%% See: http://javadoc.google-http-java-client.googlecode.com/hg/1.18.0-rc/com/google/api/client/util/ExponentialBackOff.html
-spec rand_increment(pos_integer()) -> pos_integer().
rand_increment(N) ->
    RandFactor = get_env(rand_factor, 0.5),
    DefMultiplier = get_env(def_multiplier, 1.5),
    Rand = 1 - RandFactor + random:uniform(),
    erlang:round(increment(N) * DefMultiplier * Rand).

-spec rand_increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
rand_increment(N, Max) -> min(rand_increment(N), Max).

%% Increments + Timer support

%% init function when the user doesn't feel like using a timer
%% provided by this library
-spec init(Start, Max) -> backoff() when
    Start :: pos_integer(),
    Max :: pos_integer() | infinity.
init(Start,Max) ->
    init(Start, Max, undefined, undefined).

%% init function when the user feels like using a timer
%% provided by this library
-spec init(Start, Max, Dest, Value) -> backoff() when
    Start :: pos_integer(),
    Max :: pos_integer() | infinity,
    Value :: term() | undefined,
    Dest :: pid() | undefined.
init(Start, Max, Dest, Value) ->
    #backoff{start=Start, current=Start, max=Max, value=Value, dest=Dest}.

%% Starts a timer from the `backoff()' argument, using erlang:start_timer/3.
%% No reference tracking is done, and this is left to the user. This function
%% is purely a convenience function.
-spec fire(backoff()) -> Timer::reference().
fire(#backoff{current=Delay, value=Value, dest=Dest}) ->
    erlang:start_timer(Delay, Dest, Value).

%% Reads the current backoff value
-spec get(backoff()) -> pos_integer().
get(#backoff{current=Delay}) -> Delay.

%% Swaps between the states of the backoff.
-spec type(backoff(), type()) -> backoff().
type(#backoff{}=B, Type) when Type =:= jitter; Type =:= decorrelated_jitter ->
    maybe_seed(),
    B#backoff{type=Type};
type(#backoff{}=B, normal) ->
    B#backoff{type=normal}.

-spec fail(backoff()) -> {New::pos_integer(), backoff()}.
fail(B=#backoff{current=Delay, max=infinity, type=normal}) ->
    NewDelay = increment(Delay),
    {NewDelay, B#backoff{current=NewDelay}};
fail(B=#backoff{current=Delay, max=Max, type=normal}) ->
    NewDelay = increment(Delay, Max),
    {NewDelay, B#backoff{current=NewDelay}};
fail(B=#backoff{current=Delay, max=infinity, type=jitter}) ->
    NewDelay = rand_increment(Delay),
    {NewDelay, B#backoff{current=NewDelay}};
fail(B=#backoff{current=Delay, max=Max, type=jitter}) ->
    NewDelay = rand_increment(Delay, Max),
    {NewDelay, B#backoff{current=NewDelay}};
%% Decorrelated Jitter, see http://www.awsarchitectureblog.com/2015/03/backoff.html
fail(B=#backoff{current=Delay, start=Start, max=Max, type=decorrelated_jitter}) ->
    NextMax = round(Delay * get_env(decorrelated_multiplier, 3)),
    NewDelay = min(Max, rand_between(Start, NextMax)),
    {NewDelay, B#backoff{current=NewDelay}}.


-spec succeed(backoff()) -> {New::pos_integer(), backoff()}.
succeed(B=#backoff{start=Start}) ->
    {Start, B#backoff{current=Start}}.

maybe_seed() ->
    case erlang:get(random_seed) of
        undefined -> random:seed(erlang:now());
        {X,X,X} -> random:seed(erlang:now());
        _ -> ok
    end.

get_env(Param, Default) ->
    case application:get_env(?MODULE, Param) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

rand_between(From, To) ->
    From2 = From - 1,
    From2 + random:uniform(To - From2).

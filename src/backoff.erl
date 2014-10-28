-module(backoff).
-export([increment/1, increment/2]).
-export([rand_increment/1, rand_increment/2]).
-export([jitter/1]).
-export([init/2, init/4,
         fire/1, get/1, succeed/1, fail/1]).
-record(backoff, {start=undefined :: pos_integer(),
                  max=undefined :: pos_integer(),
                  current=undefined :: pos_integer(),
                  jitter=false :: boolean(),
                  value :: term(),
                  dest :: pid()}).

-define(RAND_FACTOR, 0.5).

-opaque backoff() :: #backoff{}.

-export_type([backoff/0]).

%% Just do the increments by hand!
-spec increment(pos_integer()) -> pos_integer().
increment(N) when is_integer(N) -> N bsl 1.

-spec increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
increment(N, Max) -> min(increment(N), Max).

%% Algorithm inspired in the Google HTTP Java client implementation of Exponential BackOff.
%% See: http://javadoc.google-http-java-client.googlecode.com/hg/1.18.0-rc/com/google/api/client/util/ExponentialBackOff.html
-spec rand_increment(pos_integer()) -> pos_integer().
rand_increment(N) ->
    Rand = 1 - ?RAND_FACTOR + random:uniform(),
    erlang:round(increment(N) * Rand).

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
    Value :: term(),
    Dest :: pid().
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

%% Toggles jitter feature.
-spec jitter(backoff()) -> backoff().
jitter(B=#backoff{jitter=false}) ->
    B#backoff{jitter=true};
jitter(B=#backoff{jitter=true}) ->
    B#backoff{jitter=false}.

-spec fail(backoff()) -> {New::pos_integer(), backoff()}.
fail(B=#backoff{current=Delay, max=infinity, jitter=false}) ->
    NewDelay = increment(Delay),
    {NewDelay, B#backoff{current=NewDelay}};
fail(B=#backoff{current=Delay, max=Max, jitter=false}) ->
    NewDelay = increment(Delay, Max),
    {NewDelay, B#backoff{current=NewDelay}};
fail(B=#backoff{current=Delay, max=infinity, jitter=true}) ->
    NewDelay = rand_increment(Delay),
    {NewDelay, B#backoff{current=NewDelay}};
fail(B=#backoff{current=Delay, max=Max, jitter=true}) ->
    NewDelay = rand_increment(Delay, Max),
    {NewDelay, B#backoff{current=NewDelay}}.


-spec succeed(backoff()) -> {New::pos_integer(), backoff()}.
succeed(B=#backoff{start=Start}) ->
    {Start, B#backoff{current=Start}}.

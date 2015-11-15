-module(backoff).
-export([increment/1, increment/2]).
-export([rand_increment/1, rand_increment/2]).
-export([type/2]).
-export([init/2, init/4,
         fire/1, get/1, succeed/1, fail/1]).

-record(backoff, {start :: pos_integer(),
                  max :: pos_integer() | infinity,
                  current :: pos_integer(),
                  type=normal :: normal | jitter,
                  value :: term(),
                  dest :: pid()}).

-opaque backoff() :: #backoff{}.

-export_type([backoff/0]).

%% Just do the increments by hand!
-spec increment(pos_integer()) -> pos_integer().
increment(N) when is_integer(N) -> N bsl 1.

-spec increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
increment(N, Max) -> min(increment(N), Max).

%% Just do the random increments by hand!
%% Choose delay uniformly from [0.5 * Time, 1.5 * Time] as recommended in:
%% Sally Floyd and Van Jacobson, The Synchronization of Periodic Routing Messages,
%% April 1994 IEEE/ACM Transactions on Networking.
%% http://ee.lbl.gov/papers/sync_94.pdf
-spec rand_increment(pos_integer()) -> pos_integer().
rand_increment(N) ->
    %% New delay chosen from [N, 3N], i.e. [0.5 * 2N, 1.5 * 2N]
    Width = N bsl 1,
    N + random:uniform(Width + 1) - 1.

-spec rand_increment(N, Max) -> pos_integer() when
    N :: pos_integer(),
    Max :: pos_integer().
rand_increment(N, Max) ->
    %% The largest interval for [0.5 * Time, 1.5 * Time] with maximum Max is
    %% [Max div 3, Max].
    MaxMinDelay = Max div 3,
    if
        MaxMinDelay =:= 0 ->
            random:uniform(Max);
        N > MaxMinDelay ->
            rand_increment(MaxMinDelay);
        true ->
            rand_increment(N)
    end.

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
-spec type(backoff(), normal | jitter) -> backoff().
type(#backoff{}=B, jitter) ->
    maybe_seed(),
    B#backoff{type=jitter};
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
    {NewDelay, B#backoff{current=NewDelay}}.


-spec succeed(backoff()) -> {New::pos_integer(), backoff()}.
succeed(B=#backoff{start=Start}) ->
    {Start, B#backoff{current=Start}}.

maybe_seed() ->
    case erlang:get(random_seed) of
        undefined -> random:seed(
                      erlang:phash2([node()]),
                      erlang:monotonic_time(),
                      erlang:unique_integer());
        {X,X,X} -> random:seed(
                      erlang:phash2([node()]),
                      erlang:monotonic_time(),
                      erlang:unique_integer());

        _ -> ok
    end.

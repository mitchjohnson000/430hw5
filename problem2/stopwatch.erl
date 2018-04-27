-module(stopwatch).
-export([time_it/1]).

%% @doc Executes the given zero-arg function and measures how long it takes.
%% @returns A 2-tuple, where the first item is the return value from F and where
%% the second item is the number of milliseconds which elapsed while running F.
time_it(F) ->
  Start = erlang:monotonic_time(millisecond),
  RetVal = F(),
  End = erlang:monotonic_time(millisecond),
  {RetVal, End-Start}.

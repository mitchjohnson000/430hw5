-module(histogram_tests).
-export([
   test_all/0,
   test_from_samples/0,
   test_merge/0,
   test_strictly_monotonic_keys/0,
   test_num_samples/0
 ]).

test_all() ->
  TestResults = [
    test_from_samples(),
    test_merge(),
    test_strictly_monotonic_keys(),
    test_num_samples()
  ],
  lists:all(fun(X) -> X end, TestResults).

test_from_samples() ->
  Samples = [1, 2, 4, 2],
  Expected = [{1, 1}, {2, 2}, {4, 1}],
  Expected == histogram:from_samples(Samples).

test_merge() ->
  Xs = [{1, 1}, {2, 1}, {4, 1}],
  Ys = [{1, 1}, {3, 1}],
  Expected = [{1, 2}, {2, 1}, {3, 1}, {4, 1}],
  Expected == histogram:merge(Xs, Ys).

test_strictly_monotonic_keys() ->
  NumSamples = 1000,
  UpperBound = 100,
  NumWorkers = 4,
  Histogram = histogram:make(NumSamples, UpperBound, NumWorkers),
  Reducer = fun
              (Cur, {ok, Prev}) when Prev < Cur -> {ok, Cur};
              (_,   {ok, _}) -> not_monotonic;
              (_,   not_monotonic) -> not_monotonic
            end,
  case lists:foldl(Reducer, {ok, 0}, Histogram) of
    {ok, _} -> true;
    not_monotonic -> false
  end.

test_num_samples() ->
  NumSamples = 1000,
  UpperBound = 100,
  NumWorkers = 4,
  Histogram = histogram:make(NumSamples, UpperBound, NumWorkers),
  histogram:count_samples(Histogram) == NumSamples.

%% Problem 2d

-module(histogram).
-export([
   merge/2,
   make/2,
   make/3,
   worker/3,
   from_samples/1,
   try_it/0,
   try_it/3,
   count_samples/1
 ]).


%% @doc Calls make/3 with the given arguments while timing it. Once this call to
%% make/3 finishes, the following are printed to the terminal: the computed
%% histogram, the time elapsed to compute it, and the total number of samples.
try_it(NumSamples, UpperBound, NumActors) ->
  F = fun() -> make(NumSamples, UpperBound, NumActors) end,
  {Histogram, Elapsed} = stopwatch:time_it(F),
  io:fwrite("~w~n", [Histogram]),
  io:fwrite("Elapsed Time: ~wms~n", [Elapsed]),
  ok.

%% @doc Calls try_it/3 with some default values.
try_it() ->
  try_it(10000000, 10000, 4).


%% @doc Makes a histogram with the given number of samples and upper bound using
%% the given number of workers running concurrently. Essentially, this spawns
%% this number of worker actors and then merges their results as they arrive.
make(NumSamples, UpperBound, NumWorkers) -> 
  create_workers(NumSamples div NumWorkers,UpperBound,NumWorkers),
  get_hist([],NumWorkers).


%% @doc Makes a histogram.
make(NumSamples, UpperBound) ->
   % Make sure that you use `randomlists:make/2`.
   from_samples(randomlists:make(NumSamples,UpperBound)).


%% @doc A helper function which is meant to be spawned by make/3 as a process 
%% to run make/2. It Calls make/2 with the given parameters and then sends the
%% results to ResultsReceiver.
worker(NumSamples, UpperBound, ResultsReceiver) ->
  ResultsReceiver ! make(NumSamples,UpperBound).

create_workers(_,_,0) -> done;
create_workers(NumSamples,Ub,NumWorkers) -> spawn(histogram,worker,[NumSamples,Ub,self()]), create_workers(NumSamples,Ub,NumWorkers - 1).

get_hist(Item,0) -> Item;
get_hist(Item,NumWorkers) -> 
  receive 
    Current -> get_hist(merge(Item,Current),NumWorkers - 1)
  end.


%% @doc Creates a histogram from a list of integer samples.
from_samples(Samples) -> create_hist(lists:sort(Samples),[]).

  
create_hist([],Group) -> lists:reverse(Group);
create_hist([H|T],[]) -> create_hist(T,[{H,1}]);
create_hist([H|T],[{Key,Value} | HTail]) when Key == H -> create_hist(T,[{Key,Value + 1} | HTail]);
create_hist([H|T], Group) -> create_hist(T,[{H,1} | Group]).


%% @doc Merges two histograms together.
merge(Xs, Ys) ->merge(Xs,Ys,[]).

merge([],[],Total) -> lists:reverse(Total);
merge([],[Item|T],Total) -> merge([],T,[Item | Total]);
merge([Item|T],[],Total) -> merge(T,[],[Item | Total]);

merge([{K1,V1} | T1],[{K2,V2} | T2], Total) when K1 > K2 -> merge([{K1,V1}|T1],T2,[{K2,V2}|Total]);
merge([{K1,V1} | T1],[{K2,V2} | T2], Total) when K1 < K2 -> merge(T1,[{K2,V2}|T2],[{K1,V1}|Total]);
merge([{K1,V1} | T1], [{_,V2} | T2], Total) -> merge(T1,T2,[{K1,V1 + V2} | Total]).


%% @doc Counts the total number of samples in the given histogram. This is just
%% the sum of all of the values in all of the 2-tuples.
count_samples(Histogram) -> count_samples(Histogram,0).
count_samples([],Total) -> Total;
count_samples([{_,Value}|T],Total) -> count_samples(T,Total + Value).
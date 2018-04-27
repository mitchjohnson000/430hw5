%%% 2a: A tail-recursive Erlang fn that computes the Nth Fibonacci number.

% TODO
-module(fib).

-export([fib/1]).

fib(N) when N < 2 -> 1;
fib(N) when N >= 2 ->fib(N-1) + fib(N-2).

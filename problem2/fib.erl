%%% 2a: A tail-recursive Erlang fn that computes the Nth Fibonacci number.

% TODO
-module(fib).

-export([fib/1]).

fib(N) -> fib_con(N, 0, 1).

fib_con(0, Result, _Next) -> Result;

fib_con(Iter, Result, Next) when Iter > 0 ->

fib_con(Iter-1, Next, Result+Next).

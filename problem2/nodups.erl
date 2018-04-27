%%% 2c: Implements de-duplication of list elements using Erlang's `foldl/3`.

% TODO: Implement `nodups/2` as specified.



% TODO: Once `nodups/2` is implemented, uncomment the following functions to
% try it out.
-module(nodups).
-export([try_it/0]).

nodups(Item,[]) -> [Item];
nodups(Item,[H|T]) when H == Item -> [H|T];
nodups(Item,List) -> [Item | List].



nodups(Xs) -> lists:reverse(lists:foldl(fun nodups/2, [], Xs)).

try_it() ->
 nodups(lists:sort(randomlists:prepend(10, 100))).

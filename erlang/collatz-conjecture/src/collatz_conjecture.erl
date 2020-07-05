-module(collatz_conjecture).

-export([steps/1]).

steps(N) when is_integer(N), N > 0 -> steps(N, 0);
steps(_) -> erlang:error(badarg).

steps(1, Count) -> Count;
steps(N, Count) when 0 =:= N rem 2 -> steps(N div 2, Count + 1);
steps(N, Count) -> steps(1 + N * 3, Count + 1).


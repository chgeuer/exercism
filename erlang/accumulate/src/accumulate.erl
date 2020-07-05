-module(accumulate).

-export([accumulate/2]).

accumulate(Fn, List) -> accumulate(Fn, List, []).

accumulate(_Fn, [], Accumulator) -> lists:reverse(Accumulator);
accumulate(Fn, [Head|Tail], Accumulator) -> accumulate(Fn, Tail, [Fn(Head) | Accumulator]).


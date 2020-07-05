-module(armstrong_numbers).

-export([is_armstrong_number/1]).

is_armstrong_number(Number) ->
    CharToInt = fun (X) -> {Num, []} = string:to_integer([X]), Num end,
    Pow = fun (Exp) -> fun (X) -> trunc(math:pow(X, Exp)) end end,
    [Str] = io_lib:format("~p", [Number]),
    Length = string:length(Str),
    Ints = lists:map(CharToInt, Str),
    Exp = lists:map(Pow(Length), Ints),
    Number =:= lists:sum(Exp).

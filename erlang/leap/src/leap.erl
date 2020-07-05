-module(leap).

-export([leap_year/1]).

leap_year(Year) -> x(
	teilbar_ohne_rest(Year, 4),
	teilbar_ohne_rest(Year, 100),
	teilbar_ohne_rest(Year, 400)) .

teilbar_ohne_rest(Year, Teiler) -> 0 =:= Year rem Teiler.

x(_, _, true) -> true;
x(_, true, _) -> false;
x(true, _, _) -> true;
x(_, _, _) -> false.


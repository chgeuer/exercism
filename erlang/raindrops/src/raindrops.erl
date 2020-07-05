-module(raindrops).

-export([convert/1]).

strIf(true, Str) -> Str;
strIf(false, _) -> "".

p(Number, Modulus, Str) -> 
	IsFactor = 0 =:= Number rem Modulus,
	strIf(IsFactor, Str).

num(Number, "") -> integer_to_list(Number);
num(_, PlingPlangPlong) -> PlingPlangPlong.

% num2(Number, PlingPlangPlong) ->
% 	case PlingPlangPlong of
% 		"" -> integer_to_list(Number);
% 		_ -> PlingPlangPlong
% 	end.

convert(Number) ->
	PlingPlangPlong = p(Number, 3, "Pling") ++ p(Number, 5, "Plang") ++ p(Number, 7, "Plong"),
	num(Number, PlingPlangPlong).

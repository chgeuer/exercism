-module(bob).

-export([response/1]).
-spec response(string()) -> string().

has_letters([]) -> false;
has_letters([H|T]) ->
	IsLowerChar = (H >= $a) and (H =< $z),
	IsUpperChar = (H >= $A) and (H =< $Z),
	IsChar = IsLowerChar or IsUpperChar,
	IsChar or has_letters(T).

response(String) ->
	S = string:trim(String),
	Reverse = string:reverse(S),
	ReverseUpper = string:uppercase(Reverse),
	HasLetters = has_letters(S),

	IsQuestion = case ReverseUpper of
	    "?" ++ _ -> true;
	    _ -> false
	end,

	{ IsEmpty, IsYelling } = case ReverseUpper of
	   "" ->      { true,  false };
	   Reverse -> { false, HasLetters };
	   _ ->       { false, false }
	end,

	case { IsEmpty, IsQuestion, IsYelling } of
		{ true, _, _ }     -> "Fine. Be that way!";
		{ _, true, false } -> "Sure.";
		{ _, true, true  } -> "Calm down, I know what I'm doing!";
		{ _, false, true } -> "Whoa, chill out!";
		_                  -> "Whatever."
	end.


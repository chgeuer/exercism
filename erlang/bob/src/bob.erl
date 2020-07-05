-module(bob).

-export([response/1]).

-spec response(string()) -> string().

% "" -> 'Fine. Be that way!'
% Ask question ->'Sure.'
% Yell -> 'Whoa, chill out!'
% Yell Question: 'Calm down, I know what I'm doing!'
% _ -> 'Whatever.'

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
	LastChar = string:substr(Reverse, 1, 1),

	X = case { ReverseUpper, HasLetters, LastChar } of
		{ "", _, _ } -> "Fine. Be that way!";
		{ Reverse, _, "?" } -> "Calm down, I know what I'm doing!";
		{ _, _, "?" } -> "Sure.";
		{ Reverse, true, _ } -> "Whoa, chill out!";
		_ -> "Whatever."
	end,
	% io:format("\"~s\" ---> \"~s\" ~n", [S, X]),
	X.

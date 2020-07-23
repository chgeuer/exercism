-module(isogram).

-export([is_isogram/1]).

-define(IS_CHAR(C), (C >= $A), (C =< $Z)).

is_isogram(Phrase) -> is_isogram(string:uppercase(Phrase), maps:new()).

is_isogram([], _) -> true ;
is_isogram([Char|Tail], Map) when ?IS_CHAR(Char) -> 
	case maps:is_key(Char, Map) of 
		true -> false;
		false -> is_isogram(Tail, Map#{ Char => true })
	end;
is_isogram([_NonChar|Tail], Map) -> is_isogram(Tail, Map).


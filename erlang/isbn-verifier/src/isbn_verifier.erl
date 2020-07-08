-module(isbn_verifier).

-compile([export_all]).
%-export([is_valid/1]).

-define(IS_DIGIT(D),(D >= $0) and (D =< $9)).

-define(IS_DIGITX(D),(D =:= $X) or ?IS_DIGIT(D)).


is_digit(D) -> ?IS_DIGIT(D).

is_digitX(D) -> ?IS_DIGITX(D).

filterX(String) when is_list(String) -> lists:filter(fun is_digitX/1, String).

to_digit(D) when ?IS_DIGIT(D) -> D - $0;
to_digit($X) -> 10.

crack([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10] = A)
	when ?IS_DIGIT(X1) and ?IS_DIGIT(X2) and ?IS_DIGIT(X3) and ?IS_DIGIT(X4)
	 and ?IS_DIGIT(X5) and ?IS_DIGIT(X6) and ?IS_DIGIT(X7) and ?IS_DIGIT(X8)
	 and ?IS_DIGIT(X9) and ?IS_DIGITX(X10) ->
	{ isbn_10, lists:map(fun to_digit/1, A) };
crack([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13] = A)
	when ?IS_DIGIT(X1) and ?IS_DIGIT(X2) and ?IS_DIGIT(X3) and ?IS_DIGIT(X4)
	 and ?IS_DIGIT(X5) and ?IS_DIGIT(X6) and ?IS_DIGIT(X7) and ?IS_DIGIT(X8)
	 and ?IS_DIGIT(X9) and ?IS_DIGIT(X10) and ?IS_DIGIT(X11) and ?IS_DIGIT(X12)
	 and ?IS_DIGITX(X13) ->
	{ isbn_13, lists:map(fun to_digit/1, A) };
crack(_) -> { error, invalid_isbn }.

is_proper_length(String) ->
	Length = length(filterX(String)),
	case Length of 10 -> true; 13 -> true; _ -> false end.

is_valid(_Isbn) -> false.

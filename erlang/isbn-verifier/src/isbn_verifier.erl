-module(isbn_verifier).

%-compile([export_all]).
-export([is_valid/1]).

-define(DEC(D),(D >= $0) and (D =< $9)).
-define(DECX(D),(D == $X) or ?DEC(D)).

filter(String) -> lists:filter(fun(D) -> ?DECX(D) end, String).

char_to_int(D) when ?DEC(D) -> D - $0;
char_to_int($X) -> 10.

to_isbn_tuple([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10] = A)
	when ?DEC(X1), ?DEC(X2), ?DEC(X3), ?DEC(X4), ?DEC(X5), 
	     ?DEC(X6), ?DEC(X7), ?DEC(X8), ?DEC(X9), ?DECX(X10) ->
	{ isbn_10, lists:map(fun char_to_int/1, A) };
to_isbn_tuple([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13] = A)
	when ?DEC(X1), ?DEC(X2), ?DEC(X3), ?DEC(X4), ?DEC(X5), 
	     ?DEC(X6), ?DEC(X7), ?DEC(X8), ?DEC(X9), ?DEC(X10), 
	     ?DEC(X11), ?DEC(X12), ?DECX(X13) ->
	{ isbn_13, lists:map(fun char_to_int/1, A) };
to_isbn_tuple(_) -> { error, invalid_isbn }.

checksum_digit({ isbn_10, [_,_,_,_,_,_,_,_,_,D]}) -> D;
checksum_digit({ isbn_13, [_,_,_,_,_,_,_,_,_,_,_,_,D]}) -> D;
checksum_digit(_) -> { error, checksum_not_found }.

compute_checksum({ isbn_10, Digits }) ->
	Multipliers = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 ],
	Pairs = lists:zip(Digits, Multipliers),
	lists:foldl(fun({A,B}, Acc) -> A*B + Acc end, 0, Pairs) rem 11;
compute_checksum({ isbn_13, Digits }) ->
	Multipliers = [ 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 0 ],
	Pairs = lists:zip(Digits, Multipliers),
	10 - (lists:foldl(fun({A,B}, Acc) -> A*B + Acc end, 0, Pairs) rem 10);
compute_checksum(String) when is_list(String) -> 
	T = to_isbn_tuple(String),
	compute_checksum(T).

is_valid(Isbn) -> 
	Filtered = filter(Isbn),
	Tuple = to_isbn_tuple(Filtered),
	case Tuple of
		{ error, _ } -> false;
		_ -> checksum_digit(Tuple) == compute_checksum(Tuple)
	end.


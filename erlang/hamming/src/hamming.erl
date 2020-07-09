-module(hamming).
-export([distance/2]).
-define(LENGTH_ERROR,{ error, "left and right strands must be of equal length" }).

distance(Strand1, Strand2) -> distance(Strand1, Strand2, 0).

distance([], [], Count) -> Count;
distance([H|T1], [H|T2], Count) -> distance(T1, T2, Count);
distance([_|T1], [_|T2], Count) -> distance(T1, T2, Count + 1);
distance([], [_], _) -> ?LENGTH_ERROR;
distance([_], [], _) -> ?LENGTH_ERROR.


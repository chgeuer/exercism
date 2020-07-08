-module(hamming).
-export([distance/2]).

distance(Strand1, Strand2) -> distance(Strand1, Strand2, 0).

distance([], [], Count) -> Count;
distance([H|T1], [H|T2], Count) -> distance(T1, T2, Count);
distance([H1|T1], [H2|T2], Count) when H1 /= H2 -> distance(T1, T2, Count + 1);
distance([], [_], _) -> { error, "left and right strands must be of equal length" };
distance([_], [], _) -> { error, "left and right strands must be of equal length" }.


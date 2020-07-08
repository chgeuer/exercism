-module(rna_transcription).
-export([to_rna/1]).

to_rna([]) -> [];
to_rna([X | T]) -> [map(X) | to_rna(T)].

map($G) -> $C;
map($C) -> $G;
map($T) -> $A;
map($A) -> $U.

-module(rna_transcription).
-export([to_rna/1]).

to_rna(Strand) -> 
	lists:map(fun map/1, Strand).

map($G) -> $C;
map($C) -> $G;
map($T) -> $A;
map($A) -> $U.


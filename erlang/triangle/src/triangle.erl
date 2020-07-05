-module(triangle).
-export([kind/3]).
-define(T(A, B, C), A >= B+C).

kind(A, B, C) when A =< 0 ; B =< 0 ; C =< 0 -> {error, "all side lengths must be positive"};
kind(A, B, C) when ?T(A,B,C); ?T(B,A,C); ?T(C,A,B) -> {error, "side lengths violate triangle inequality"};
kind(A, A, A) -> equilateral;
kind(A, A, _) -> isosceles;
kind(A, _, A) -> isosceles;
kind(_, A, A) -> isosceles;
kind(_, _, _) -> scalene.


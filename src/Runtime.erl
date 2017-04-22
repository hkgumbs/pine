-module('Runtime').

-export([ apply/2, apply/3, apply/4, apply/5, apply/6, apply/7, apply/8, apply/9, apply/10 ]).


apply(F, A) when is_function(F, 1) -> F(A).
apply(F, A1, A2) when is_function(F, 2) -> F(A1, A2).
apply(F, A1, A2, A3) when is_function(F, 3) -> F(A1, A2, A3).
apply(F, A1, A2, A3, A4) when is_function(F, 4) -> F(A1, A2, A3, A4).
apply(F, A1, A2, A3, A4, A5) when is_function(F, 5) -> F(A1, A2, A3, A4, A5).
apply(F, A1, A2, A3, A4, A5, A6) when is_function(F, 6) -> F(A1, A2, A3, A4, A5, A6).
apply(F, A1, A2, A3, A4, A5, A6, A7) when is_function(F, 7) -> F(A1, A2, A3, A4, A5, A6, A7).
apply(F, A1, A2, A3, A4, A5, A6, A7, A8) when is_function(F, 8) -> F(A1, A2, A3, A4, A5, A6, A7, A8).
apply(F, A1, A2, A3, A4, A5, A6, A7, A8, A9) when is_function(F, 9) -> F(A1, A2, A3, A4, A5, A6, A7, A8, A9).

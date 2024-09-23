-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((EVF) -> EVG), fun((EVG) -> EVH)) -> fun((EVF) -> EVH).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 7).
-spec curry2(fun((EVI, EVJ) -> EVK)) -> fun((EVI) -> fun((EVJ) -> EVK)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 12).
-spec curry3(fun((EVM, EVN, EVO) -> EVP)) -> fun((EVM) -> fun((EVN) -> fun((EVO) -> EVP))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 17).
-spec curry4(fun((EVR, EVS, EVT, EVU) -> EVV)) -> fun((EVR) -> fun((EVS) -> fun((EVT) -> fun((EVU) -> EVV)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 22).
-spec curry5(fun((EVX, EVY, EVZ, EWA, EWB) -> EWC)) -> fun((EVX) -> fun((EVY) -> fun((EVZ) -> fun((EWA) -> fun((EWB) -> EWC))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 27).
-spec curry6(fun((EWE, EWF, EWG, EWH, EWI, EWJ) -> EWK)) -> fun((EWE) -> fun((EWF) -> fun((EWG) -> fun((EWH) -> fun((EWI) -> fun((EWJ) -> EWK)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 36).
-spec flip(fun((EWM, EWN) -> EWO)) -> fun((EWN, EWM) -> EWO).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 42).
-spec identity(EWP) -> EWP.
identity(X) ->
    X.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 47).
-spec constant(EWQ) -> fun((any()) -> EWQ).
constant(Value) ->
    fun(_) -> Value end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 56).
-spec tap(EWS, fun((EWS) -> any())) -> EWS.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 62).
-spec apply1(fun((EWU) -> EWV), EWU) -> EWV.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 67).
-spec apply2(fun((EWW, EWX) -> EWY), EWW, EWX) -> EWY.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/function.gleam", 72).
-spec apply3(fun((EWZ, EXA, EXB) -> EXC), EWZ, EXA, EXB) -> EXC.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).

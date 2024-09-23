-module(gleam@bool).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export(['and'/2, 'or'/2, negate/1, nor/2, nand/2, exclusive_or/2, exclusive_nor/2, compare/2, to_int/1, to_string/1, guard/3, lazy_guard/3]).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 33).
-spec 'and'(boolean(), boolean()) -> boolean().
'and'(A, B) ->
    A andalso B.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 59).
-spec 'or'(boolean(), boolean()) -> boolean().
'or'(A, B) ->
    A orelse B.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 79).
-spec negate(boolean()) -> boolean().
negate(Bool) ->
    case Bool of
        true ->
            false;

        false ->
            true
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 110).
-spec nor(boolean(), boolean()) -> boolean().
nor(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            false;

        {true, false} ->
            false;

        {true, true} ->
            false
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 143).
-spec nand(boolean(), boolean()) -> boolean().
nand(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            true;

        {true, false} ->
            true;

        {true, true} ->
            false
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 176).
-spec exclusive_or(boolean(), boolean()) -> boolean().
exclusive_or(A, B) ->
    case {A, B} of
        {false, false} ->
            false;

        {false, true} ->
            true;

        {true, false} ->
            true;

        {true, true} ->
            false
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 209).
-spec exclusive_nor(boolean(), boolean()) -> boolean().
exclusive_nor(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            false;

        {true, false} ->
            false;

        {true, true} ->
            true
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 229).
-spec compare(boolean(), boolean()) -> gleam@order:order().
compare(A, B) ->
    case {A, B} of
        {true, true} ->
            eq;

        {true, false} ->
            gt;

        {false, false} ->
            eq;

        {false, true} ->
            lt
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 252).
-spec to_int(boolean()) -> integer().
to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 273).
-spec to_string(boolean()) -> binary().
to_string(Bool) ->
    case Bool of
        false ->
            <<"False"/utf8>>;

        true ->
            <<"True"/utf8>>
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 332).
-spec guard(boolean(), DKR, fun(() -> DKR)) -> DKR.
guard(Requirement, Consequence, Alternative) ->
    case Requirement of
        true ->
            Consequence;

        false ->
            Alternative()
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/bool.gleam", 373).
-spec lazy_guard(boolean(), fun(() -> DKS), fun(() -> DKS)) -> DKS.
lazy_guard(Requirement, Consequence, Alternative) ->
    case Requirement of
        true ->
            Consequence();

        false ->
            Alternative()
    end.

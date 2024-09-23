-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/pair.gleam", 10).
-spec first({AAK, any()}) -> AAK.
first(Pair) ->
    {A, _} = Pair,
    A.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/pair.gleam", 24).
-spec second({any(), AAN}) -> AAN.
second(Pair) ->
    {_, A} = Pair,
    A.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/pair.gleam", 38).
-spec swap({AAO, AAP}) -> {AAP, AAO}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/pair.gleam", 53).
-spec map_first({AAQ, AAR}, fun((AAQ) -> AAS)) -> {AAS, AAR}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/pair.gleam", 68).
-spec map_second({AAT, AAU}, fun((AAU) -> AAV)) -> {AAT, AAV}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/pair.gleam", 83).
-spec new(AAW, AAX) -> {AAW, AAX}.
new(First, Second) ->
    {First, Second}.

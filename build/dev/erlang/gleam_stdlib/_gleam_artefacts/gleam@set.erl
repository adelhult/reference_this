-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FDU) :: {set, gleam@dict:dict(FDU, list(nil))}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 32).
-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 50).
-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 68).
-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 110).
-spec contains(set(FEF), FEF) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 131).
-spec delete(set(FEH), FEH) -> set(FEH).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 149).
-spec to_list(set(FEK)) -> list(FEK).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 190).
-spec fold(set(FEQ), FES, fun((FES, FEQ) -> FES)) -> FES.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 214).
-spec filter(set(FET), fun((FET) -> boolean())) -> set(FET).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 249).
-spec drop(set(FFA), list(FFA)) -> set(FFA).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 267).
-spec take(set(FFE), list(FFE)) -> set(FFE).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 271).
-spec order(set(FFI), set(FFI)) -> {set(FFI), set(FFI)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 305).
-spec intersection(set(FFR), set(FFR)) -> set(FFR).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 323).
-spec difference(set(FFV), set(FFV)) -> set(FFV).
difference(First, Second) ->
    drop(First, to_list(Second)).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 344).
-spec is_subset(set(FFZ), set(FFZ)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 362).
-spec is_disjoint(set(FGC), set(FGC)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 86).
-spec insert(set(FEC), FEC) -> set(FEC).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 167).
-spec from_list(list(FEN)) -> set(FEN).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 232).
-spec map(set(FEW), fun((FEW) -> FEY)) -> set(FEY).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 289).
-spec union(set(FFN), set(FFN)) -> set(FFN).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/set.gleam", 374).
-spec symmetric_difference(set(FGF), set(FGF)) -> set(FGF).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).

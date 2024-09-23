-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, is_empty/1, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, upsert/3, fold/3, map_values/2, filter/2, each/2, combine/3]).
-export_type([dict/2]).

-type dict(KX, KY) :: any() | {gleam_phantom, KX, KY}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 36).
-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 80).
-spec to_list(dict(LH, LI)) -> list({LH, LI}).
to_list(Dict) ->
    maps:to_list(Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 127).
-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 52).
-spec is_empty(dict(any(), any())) -> boolean().
is_empty(Dict) ->
    Dict =:= new().

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 152).
-spec get(dict(MO, MP), MO) -> {ok, MP} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 116).
-spec has_key(dict(LY, any()), LY) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 177).
-spec insert(dict(NA, NB), NA, NB) -> dict(NA, NB).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 92).
-spec fold_list_of_pair(list({LR, LS}), dict(LR, LS)) -> dict(LR, LS).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 88).
-spec from_list(list({LM, LN})) -> dict(LM, LN).
from_list(List) ->
    maps:from_list(List).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 230).
-spec reverse_and_concat(list(UQ), list(UQ)) -> list(UQ).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 237).
-spec do_keys_acc(list({ON, any()}), list(ON)) -> list(ON).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 220).
-spec keys(dict(OA, any())) -> list(OA).
keys(Dict) ->
    maps:keys(Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 267).
-spec do_values_acc(list({any(), PD}), list(PD)) -> list(PD).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 257).
-spec values(dict(any(), OT)) -> list(OT).
values(Dict) ->
    maps:values(Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 339).
-spec insert_taken(dict(QH, QI), list(QH), dict(QH, QI)) -> dict(QH, QI).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 330).
-spec take(dict(PT, PU), list(PT)) -> dict(PT, PU).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 381).
-spec insert_pair(dict(RG, RH), {RG, RH}) -> dict(RG, RH).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 385).
-spec fold_inserts(list({RM, RN}), dict(RM, RN)) -> dict(RM, RN).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 370).
-spec merge(dict(QQ, QR), dict(QQ, QR)) -> dict(QQ, QR).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 407).
-spec delete(dict(RT, RU), RT) -> dict(RT, RU).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 435).
-spec drop(dict(SF, SG), list(SF)) -> dict(SF, SG).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 465).
-spec upsert(dict(SM, SN), SM, fun((gleam@option:option(SN)) -> SN)) -> dict(SM, SN).
upsert(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 477).
-spec do_fold(list({ST, SU}), SW, fun((SW, ST, SU) -> SW)) -> SW.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 509).
-spec fold(dict(SX, SY), TB, fun((TB, SX, SY) -> TB)) -> TB.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 196).
-spec map_values(dict(NM, NN), fun((NM, NN) -> NQ)) -> dict(NM, NQ).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 291).
-spec filter(dict(PH, PI), fun((PH, PI) -> boolean())) -> dict(PH, PI).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 541).
-spec each(dict(TC, TD), fun((TC, TD) -> any())) -> nil.
each(Dict, Fun) ->
    fold(
        Dict,
        nil,
        fun(Nil, K, V) ->
            Fun(K, V),
            Nil
        end
    ).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/dict.gleam", 562).
-spec combine(dict(TH, TI), dict(TH, TI), fun((TI, TI) -> TI)) -> dict(TH, TI).
combine(Dict, Other, Fun) ->
    fold(Dict, Other, fun(Acc, Key, Value) -> case get(Acc, Key) of
                {ok, Other_value} ->
                    insert(Acc, Key, Fun(Value, Other_value));

                {error, _} ->
                    insert(Acc, Key, Value)
            end end).

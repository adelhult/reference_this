-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 20).
-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 41).
-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 66).
-spec map({ok, BNX} | {error, BNY}, fun((BNX) -> BOB)) -> {ok, BOB} |
    {error, BNY}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 91).
-spec map_error({ok, BOE} | {error, BOF}, fun((BOF) -> BOI)) -> {ok, BOE} |
    {error, BOI}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 120).
-spec flatten({ok, {ok, BOL} | {error, BOM}} | {error, BOM}) -> {ok, BOL} |
    {error, BOM}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 158).
-spec 'try'({ok, BOT} | {error, BOU}, fun((BOT) -> {ok, BOX} | {error, BOU})) -> {ok,
        BOX} |
    {error, BOU}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 170).
-spec then({ok, BPC} | {error, BPD}, fun((BPC) -> {ok, BPG} | {error, BPD})) -> {ok,
        BPG} |
    {error, BPD}.
then(Result, Fun) ->
    'try'(Result, Fun).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 192).
-spec unwrap({ok, BPL} | {error, any()}, BPL) -> BPL.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 214).
-spec lazy_unwrap({ok, BPP} | {error, any()}, fun(() -> BPP)) -> BPP.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 236).
-spec unwrap_error({ok, any()} | {error, BPU}, BPU) -> BPU.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 258).
-spec unwrap_both({ok, BPX} | {error, BPX}) -> BPX.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 279).
-spec nil_error({ok, BQA} | {error, any()}) -> {ok, BQA} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 307).
-spec 'or'({ok, BQG} | {error, BQH}, {ok, BQG} | {error, BQH}) -> {ok, BQG} |
    {error, BQH}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 338).
-spec lazy_or({ok, BQO} | {error, BQP}, fun(() -> {ok, BQO} | {error, BQP})) -> {ok,
        BQO} |
    {error, BQP}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 364).
-spec all(list({ok, BQW} | {error, BQX})) -> {ok, list(BQW)} | {error, BQX}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 384).
-spec do_partition(list({ok, BRL} | {error, BRM}), list(BRL), list(BRM)) -> {list(BRL),
    list(BRM)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 380).
-spec partition(list({ok, BRE} | {error, BRF})) -> {list(BRE), list(BRF)}.
partition(Results) ->
    do_partition(Results, [], []).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 406).
-spec replace({ok, any()} | {error, BRU}, BRX) -> {ok, BRX} | {error, BRU}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 427).
-spec replace_error({ok, BSA} | {error, any()}, BSE) -> {ok, BSA} | {error, BSE}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 443).
-spec values(list({ok, BSH} | {error, any()})) -> list(BSH).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/result.gleam", 474).
-spec try_recover(
    {ok, BSN} | {error, BSO},
    fun((BSO) -> {ok, BSN} | {error, BSR})
) -> {ok, BSN} | {error, BSR}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.

-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(GG) :: {some, GG} | none.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 24).
-spec do_all(list(option(GH)), list(GH)) -> option(list(GH)).
do_all(List, Acc) ->
    case List of
        [] ->
            {some, Acc};

        [X | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case {Acc@1, Item} of
                    {{some, Values}, {some, Value}} ->
                        {some, [Value | Values]};

                    {_, _} ->
                        none
                end end,
            Accumulate(do_all(Rest, Acc), X)
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 55).
-spec all(list(option(GN))) -> option(list(GN)).
all(List) ->
    do_all(List, []).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 73).
-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 91).
-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 109).
-spec to_result(option(GW), GZ) -> {ok, GW} | {error, GZ}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 130).
-spec from_result({ok, HC} | {error, any()}) -> option(HC).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 151).
-spec unwrap(option(HH), HH) -> HH.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 172).
-spec lazy_unwrap(option(HJ), fun(() -> HJ)) -> HJ.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 197).
-spec map(option(HL), fun((HL) -> HN)) -> option(HN).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 223).
-spec flatten(option(option(HP))) -> option(HP).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 262).
-spec then(option(HT), fun((HT) -> option(HV))) -> option(HV).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 293).
-spec 'or'(option(HY), option(HY)) -> option(HY).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 324).
-spec lazy_or(option(IC), fun(() -> option(IC))) -> option(IC).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 331).
-spec do_values(list(option(IG)), list(IG)) -> list(IG).
do_values(List, Acc) ->
    case List of
        [] ->
            Acc;

        [X | Xs] ->
            Accumulate = fun(Acc@1, Item) -> case Item of
                    {some, Value} ->
                        [Value | Acc@1];

                    none ->
                        Acc@1
                end end,
            Accumulate(do_values(Xs, Acc), X)
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/option.gleam", 356).
-spec values(list(option(IL))) -> list(IL).
values(Options) ->
    do_values(Options, []).

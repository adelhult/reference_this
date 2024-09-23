-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(BXQ) :: stop | {continue, BXQ, fun(() -> action(BXQ))}.

-opaque iterator(BXR) :: {iterator, fun(() -> action(BXR))}.

-type step(BXS, BXT) :: {next, BXS, BXT} | done.

-type chunk(BXU, BXV) :: {another_by,
        list(BXU),
        BXV,
        BXU,
        fun(() -> action(BXU))} |
    {last_by, list(BXU)}.

-type sized_chunk(BXW) :: {another, list(BXW), fun(() -> action(BXW))} |
    {last, list(BXW)} |
    no_more.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 38).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 43).
-spec do_unfold(BXZ, fun((BXZ) -> step(BYA, BXZ))) -> fun(() -> action(BYA)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 76).
-spec unfold(BYE, fun((BYE) -> step(BYF, BYE))) -> iterator(BYF).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 95).
-spec repeatedly(fun(() -> BYJ)) -> iterator(BYJ).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 110).
-spec repeat(BYL) -> iterator(BYL).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 124).
-spec from_list(list(BYN)) -> iterator(BYN).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 135).
-spec do_transform(
    fun(() -> action(BYQ)),
    BYS,
    fun((BYS, BYQ) -> step(BYT, BYS))
) -> fun(() -> action(BYT)).
do_transform(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, do_transform(Next, Next_state, F)}
                end
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 170).
-spec transform(iterator(BYX), BYZ, fun((BYZ, BYX) -> step(BZA, BYZ))) -> iterator(BZA).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 179).
-spec do_fold(fun(() -> action(BZE)), fun((BZG, BZE) -> BZG), BZG) -> BZG.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 207).
-spec fold(iterator(BZH), BZJ, fun((BZJ, BZH) -> BZJ)) -> BZJ.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 221).
-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 239).
-spec to_list(iterator(BZM)) -> list(BZM).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 267).
-spec step(iterator(BZP)) -> step(BZP, iterator(BZP)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 274).
-spec do_take(fun(() -> action(BZU)), integer()) -> fun(() -> action(BZU)).
do_take(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, do_take(Next, Desired - 1)}
                end
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 307).
-spec take(iterator(BZX), integer()) -> iterator(BZX).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 313).
-spec do_drop(fun(() -> action(CAA)), integer()) -> action(CAA).
do_drop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    do_drop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 349).
-spec drop(iterator(CAD), integer()) -> iterator(CAD).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 354).
-spec do_map(fun(() -> action(CAG)), fun((CAG) -> CAI)) -> fun(() -> action(CAI)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 380).
-spec map(iterator(CAK), fun((CAK) -> CAM)) -> iterator(CAM).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 386).
-spec do_map2(
    fun(() -> action(CAO)),
    fun(() -> action(CAQ)),
    fun((CAO, CAQ) -> CAS)
) -> fun(() -> action(CAS)).
do_map2(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), do_map2(Next_a, Next_b, Fun)}
                end
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 427).
-spec map2(iterator(CAU), iterator(CAW), fun((CAU, CAW) -> CAY)) -> iterator(CAY).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = do_map2(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 436).
-spec do_append(fun(() -> action(CBA)), fun(() -> action(CBA))) -> action(CBA).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 457).
-spec append(iterator(CBE), iterator(CBE)) -> iterator(CBE).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 462).
-spec do_flatten(fun(() -> action(iterator(CBI)))) -> action(CBI).
do_flatten(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            do_append(
                erlang:element(2, It),
                fun() -> do_flatten(Next_iterator) end
            )
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 485).
-spec flatten(iterator(iterator(CBM))) -> iterator(CBM).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 505).
-spec concat(list(iterator(CBQ))) -> iterator(CBQ).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 527).
-spec flat_map(iterator(CBU), fun((CBU) -> iterator(CBW))) -> iterator(CBW).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 536).
-spec do_filter(fun(() -> action(CBZ)), fun((CBZ) -> boolean())) -> action(CBZ).
do_filter(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> do_filter(Iterator, Predicate) end};

                false ->
                    do_filter(Iterator, Predicate)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 569).
-spec filter(iterator(CCC), fun((CCC) -> boolean())) -> iterator(CCC).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 577).
-spec do_filter_map(
    fun(() -> action(CCF)),
    fun((CCF) -> {ok, CCH} | {error, any()})
) -> action(CCH).
do_filter_map(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> do_filter_map(Next, F) end};

                {error, _} ->
                    do_filter_map(Next, F)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 613).
-spec filter_map(iterator(CCM), fun((CCM) -> {ok, CCO} | {error, any()})) -> iterator(CCO).
filter_map(Iterator, F) ->
    _pipe = fun() -> do_filter_map(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 633).
-spec cycle(iterator(CCT)) -> iterator(CCT).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 679).
-spec do_find(fun(() -> action(CCX)), fun((CCX) -> boolean())) -> {ok, CCX} |
    {error, nil}.
do_find(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    do_find(Next, F)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 713).
-spec find(iterator(CDB), fun((CDB) -> boolean())) -> {ok, CDB} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 721).
-spec do_find_map(
    fun(() -> action(CDF)),
    fun((CDF) -> {ok, CDH} | {error, any()})
) -> {ok, CDH} | {error, nil}.
do_find_map(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    do_find_map(Next, F)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 758).
-spec find_map(iterator(CDN), fun((CDN) -> {ok, CDP} | {error, any()})) -> {ok,
        CDP} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find_map(_pipe, Is_desired).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 766).
-spec do_index(fun(() -> action(CDV)), integer()) -> fun(() -> action({CDV,
    integer()})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, do_index(Continuation@1, Next + 1)}
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 788).
-spec index(iterator(CDY)) -> iterator({CDY, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 803).
-spec iterate(CEB, fun((CEB) -> CEB)) -> iterator(CEB).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 810).
-spec do_take_while(fun(() -> action(CED)), fun((CED) -> boolean())) -> fun(() -> action(CED)).
do_take_while(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, do_take_while(Next, Predicate)}
                end
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 837).
-spec take_while(iterator(CEG), fun((CEG) -> boolean())) -> iterator(CEG).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 846).
-spec do_drop_while(fun(() -> action(CEJ)), fun((CEJ) -> boolean())) -> action(CEJ).
do_drop_while(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    do_drop_while(Next, Predicate)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 872).
-spec drop_while(iterator(CEM), fun((CEM) -> boolean())) -> iterator(CEM).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 880).
-spec do_scan(fun(() -> action(CEP)), fun((CER, CEP) -> CER), CER) -> fun(() -> action(CER)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 910).
-spec scan(iterator(CET), CEV, fun((CEV, CET) -> CEV)) -> iterator(CEV).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 920).
-spec do_zip(fun(() -> action(CEX)), fun(() -> action(CEZ))) -> fun(() -> action({CEX,
    CEZ})).
do_zip(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            do_zip(Next_left, Next_right)}
                end
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 949).
-spec zip(iterator(CFC), iterator(CFE)) -> iterator({CFC, CFE}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 960).
-spec next_chunk(fun(() -> action(CFH)), fun((CFH) -> CFJ), CFJ, list(CFH)) -> chunk(CFH, CFJ).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 978).
-spec do_chunk(fun(() -> action(CFN)), fun((CFN) -> CFP), CFP, CFN) -> action(list(CFN)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1003).
-spec chunk(iterator(CFS), fun((CFS) -> any())) -> iterator(list(CFS)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1023).
-spec next_sized_chunk(fun(() -> action(CFX)), integer(), list(CFX)) -> sized_chunk(CFX).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1044).
-spec do_sized_chunk(fun(() -> action(CGB)), integer()) -> fun(() -> action(list(CGB))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1081).
-spec sized_chunk(iterator(CGF), integer()) -> iterator(list(CGF)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1090).
-spec do_intersperse(fun(() -> action(CGJ)), CGJ) -> action(CGJ).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1129).
-spec intersperse(iterator(CGM), CGM) -> iterator(CGM).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1142).
-spec do_any(fun(() -> action(CGP)), fun((CGP) -> boolean())) -> boolean().
do_any(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    do_any(Next, Predicate)
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1183).
-spec any(iterator(CGR), fun((CGR) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1191).
-spec do_all(fun(() -> action(CGT)), fun((CGT) -> boolean())) -> boolean().
do_all(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    do_all(Next, Predicate);

                false ->
                    false
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1232).
-spec all(iterator(CGV), fun((CGV) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1240).
-spec update_group_with(CGX) -> fun((gleam@option:option(list(CGX))) -> list(CGX)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1249).
-spec group_updater(fun((CHB) -> CHC)) -> fun((gleam@dict:dict(CHC, list(CHB)), CHB) -> gleam@dict:dict(CHC, list(CHB))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1271).
-spec group(iterator(CHJ), fun((CHJ) -> CHL)) -> gleam@dict:dict(CHL, list(CHJ)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1301).
-spec reduce(iterator(CHP), fun((CHP, CHP) -> CHP)) -> {ok, CHP} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1331).
-spec last(iterator(CHT)) -> {ok, CHT} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1345).
-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1358).
-spec once(fun(() -> CHZ)) -> iterator(CHZ).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 658).
-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1372).
-spec single(CIB) -> iterator(CIB).
single(Elem) ->
    once(fun() -> Elem end).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1376).
-spec do_interleave(fun(() -> action(CID)), fun(() -> action(CID))) -> action(CID).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1406).
-spec interleave(iterator(CIH), iterator(CIH)) -> iterator(CIH).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1414).
-spec do_fold_until(
    fun(() -> action(CIL)),
    fun((CIN, CIL) -> gleam@list:continue_or_stop(CIN)),
    CIN
) -> CIN.
do_fold_until(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    do_fold_until(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1453).
-spec fold_until(
    iterator(CIP),
    CIR,
    fun((CIR, CIP) -> gleam@list:continue_or_stop(CIR))
) -> CIR.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1462).
-spec do_try_fold(
    fun(() -> action(CIT)),
    fun((CIV, CIT) -> {ok, CIV} | {error, CIW}),
    CIV
) -> {ok, CIV} | {error, CIW}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            gleam@result:'try'(
                F(Accumulator, Elem),
                fun(Accumulator@1) -> do_try_fold(Next, F, Accumulator@1) end
            )
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1495).
-spec try_fold(iterator(CJB), CJD, fun((CJD, CJB) -> {ok, CJD} | {error, CJE})) -> {ok,
        CJD} |
    {error, CJE}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1518).
-spec first(iterator(CJJ)) -> {ok, CJJ} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1548).
-spec at(iterator(CJN), integer()) -> {ok, CJN} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1554).
-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1578).
-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1600).
-spec each(iterator(CJV), fun((CJV) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("/home/eli/dev/reference/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1625).
-spec yield(CJY, fun(() -> iterator(CJY))) -> iterator(CJY).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.

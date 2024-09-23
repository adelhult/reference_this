-module(glint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([pretty_help/2, with_name/2, without_exit/1, as_module/1, with_indent_width/2, with_max_output_width/2, with_min_first_column_width/2, with_column_gap/2, global_help/2, command_help/2, unnamed_args/2, named_arg/2, default_pretty_help/0, flag_constraint/2, flag_help/2, flag_default/2, flag/2, command/1, get_flag/2, int_flag/1, ints_flag/1, bool_flag/1, string_flag/1, strings_flag/1, float_flag/1, floats_flag/1, path_help/3, add/3, group_flag/3, new/0, execute/2, run_and_handle/3, run/2]).
-export_type([config/0, pretty_help/0, glint/1, args_count/0, command/1, internal_command/1, named_args/0, command_node/1, out/1, value/0, flag/1, flag_internals/1, flag_entry/0, flags/0]).

-type config() :: {config,
        gleam@option:option(pretty_help()),
        gleam@option:option(binary()),
        boolean(),
        gleam@option:option(binary()),
        boolean(),
        integer(),
        integer(),
        integer(),
        integer()}.

-type pretty_help() :: {pretty_help,
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour()}.

-opaque glint(GTF) :: {glint, config(), command_node(GTF)}.

-type args_count() :: {eq_args, integer()} | {min_args, integer()}.

-opaque command(GTG) :: {command,
        fun((named_args(), list(binary()), flags()) -> GTG),
        flags(),
        binary(),
        gleam@option:option(args_count()),
        list(binary())}.

-type internal_command(GTH) :: {internal_command,
        fun((named_args(), list(binary()), flags()) -> GTH),
        flags(),
        gleam@option:option(args_count()),
        list(binary())}.

-opaque named_args() :: {named_args, gleam@dict:dict(binary(), binary())}.

-type command_node(GTI) :: {command_node,
        gleam@option:option(internal_command(GTI)),
        gleam@dict:dict(binary(), command_node(GTI)),
        flags(),
        binary()}.

-type out(GTJ) :: {out, GTJ} | {help, binary()}.

-type value() :: {b, flag_internals(boolean())} |
    {i, flag_internals(integer())} |
    {li, flag_internals(list(integer()))} |
    {f, flag_internals(float())} |
    {lf, flag_internals(list(float()))} |
    {s, flag_internals(binary())} |
    {ls, flag_internals(list(binary()))}.

-opaque flag(GTK) :: {flag,
        binary(),
        binary(),
        fun((binary()) -> {ok, GTK} | {error, snag:snag()}),
        fun((flag_internals(GTK)) -> value()),
        fun((flags(), binary()) -> {ok, GTK} | {error, snag:snag()}),
        gleam@option:option(GTK)}.

-type flag_internals(GTL) :: {flag_internals,
        gleam@option:option(GTL),
        fun((binary()) -> {ok, GTL} | {error, snag:snag()})}.

-type flag_entry() :: {flag_entry, value(), binary()}.

-opaque flags() :: {flags, gleam@dict:dict(binary(), flag_entry())}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 61).
-spec config(glint(GTS), config()) -> glint(GTS).
config(Glint, Config) ->
    erlang:setelement(2, Glint, Config).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 69).
-spec pretty_help(glint(GTV), pretty_help()) -> glint(GTV).
pretty_help(Glint, Pretty) ->
    config(
        Glint,
        erlang:setelement(2, erlang:element(2, Glint), {some, Pretty})
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 77).
-spec with_name(glint(GTY), binary()) -> glint(GTY).
with_name(Glint, Name) ->
    config(Glint, erlang:setelement(3, erlang:element(2, Glint), {some, Name})).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 85).
-spec without_exit(glint(GUB)) -> glint(GUB).
without_exit(Glint) ->
    erlang:setelement(
        2,
        Glint,
        erlang:setelement(6, erlang:element(2, Glint), false)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 93).
-spec as_module(glint(GUE)) -> glint(GUE).
as_module(Glint) ->
    config(Glint, erlang:setelement(4, erlang:element(2, Glint), true)).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 102).
-spec with_indent_width(glint(GUH), integer()) -> glint(GUH).
with_indent_width(Glint, Width) ->
    erlang:setelement(
        2,
        Glint,
        erlang:setelement(7, erlang:element(2, Glint), Width)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 110).
-spec with_max_output_width(glint(GUK), integer()) -> glint(GUK).
with_max_output_width(Glint, Width) ->
    erlang:setelement(
        2,
        Glint,
        erlang:setelement(8, erlang:element(2, Glint), Width)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 118).
-spec with_min_first_column_width(glint(GUN), integer()) -> glint(GUN).
with_min_first_column_width(Glint, Width) ->
    erlang:setelement(
        2,
        Glint,
        erlang:setelement(9, erlang:element(2, Glint), Width)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 126).
-spec with_column_gap(glint(GUQ), integer()) -> glint(GUQ).
with_column_gap(Glint, Gap) ->
    erlang:setelement(
        2,
        Glint,
        erlang:setelement(10, erlang:element(2, Glint), Gap)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 243).
-spec global_help(glint(GUZ), binary()) -> glint(GUZ).
global_help(Glint, Description) ->
    erlang:setelement(
        2,
        Glint,
        erlang:setelement(5, erlang:element(2, Glint), {some, Description})
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 292).
-spec sanitize_path(list(binary())) -> list(binary()).
sanitize_path(Path) ->
    _pipe = Path,
    _pipe@1 = gleam@list:map(_pipe, fun gleam@string:trim/1),
    gleam@list:filter(_pipe@1, fun(S) -> S /= <<""/utf8>> end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 329).
-spec command_help(binary(), fun(() -> command(GVO))) -> command(GVO).
command_help(Desc, F) ->
    erlang:setelement(4, F(), Desc).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 348).
-spec unnamed_args(args_count(), fun(() -> command(GVR))) -> command(GVR).
unnamed_args(Count, F) ->
    erlang:setelement(5, F(), {some, Count}).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 376).
-spec named_arg(
    binary(),
    fun((fun((named_args()) -> binary())) -> command(GVU))
) -> command(GVU).
named_arg(Name, F) ->
    Cmd = (F(
        fun(Named_args) ->
            _assert_subject = gleam@dict:get(
                erlang:element(2, Named_args),
                Name
            ),
            {ok, Arg} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"glint"/utf8>>,
                                function => <<"named_arg"/utf8>>,
                                line => 383})
            end,
            Arg
        end
    )),
    erlang:setelement(6, Cmd, [Name | erlang:element(6, Cmd)]).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 515).
-spec args_compare(args_count(), integer()) -> {ok, nil} | {error, snag:snag()}.
args_compare(Expected, Actual) ->
    gleam@result:map_error(case Expected of
            {eq_args, Expected@1} when Actual =:= Expected@1 ->
                {ok, nil};

            {min_args, Expected@2} when Actual >= Expected@2 ->
                {ok, nil};

            {eq_args, Expected@3} ->
                {error, gleam@int:to_string(Expected@3)};

            {min_args, Expected@4} ->
                {error,
                    <<"at least "/utf8,
                        (gleam@int:to_string(Expected@4))/binary>>}
        end, fun(Err) ->
            snag:new(
                <<<<<<"expected: "/utf8, Err/binary>>/binary,
                        " argument(s), provided: "/utf8>>/binary,
                    (gleam@int:to_string(Actual))/binary>>
            )
        end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 635).
-spec default_pretty_help() -> pretty_help().
default_pretty_help() ->
    _assert_subject = gleam_community@colour:from_rgb255(182, 255, 234),
    {ok, Usage_colour} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 636})
    end,
    _assert_subject@1 = gleam_community@colour:from_rgb255(255, 175, 243),
    {ok, Flags_colour} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 637})
    end,
    _assert_subject@2 = gleam_community@colour:from_rgb255(252, 226, 174),
    {ok, Subcommands_colour} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@2,
                        module => <<"glint"/utf8>>,
                        function => <<"default_pretty_help"/utf8>>,
                        line => 638})
    end,
    {pretty_help, Usage_colour, Flags_colour, Subcommands_colour}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 710).
-spec flag_type_info(flag_entry()) -> binary().
flag_type_info(Flag) ->
    case erlang:element(2, Flag) of
        {i, _} ->
            <<"INT"/utf8>>;

        {b, _} ->
            <<"BOOL"/utf8>>;

        {f, _} ->
            <<"FLOAT"/utf8>>;

        {lf, _} ->
            <<"FLOAT_LIST"/utf8>>;

        {li, _} ->
            <<"INT_LIST"/utf8>>;

        {ls, _} ->
            <<"STRING_LIST"/utf8>>;

        {s, _} ->
            <<"STRING"/utf8>>
    end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 737).
-spec build_subcommands_help(gleam@dict:dict(binary(), command_node(any()))) -> list(glint@internal@help:metadata()).
build_subcommands_help(Subcommands) ->
    gleam@dict:fold(
        Subcommands,
        [],
        fun(Acc, Name, Node) ->
            [{metadata, Name, erlang:element(5, Node)} | Acc]
        end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 886).
-spec new_builder(
    binary(),
    fun((flag_internals(GYJ)) -> value()),
    fun((flags(), binary()) -> {ok, GYJ} | {error, snag:snag()}),
    fun((binary()) -> {ok, GYJ} | {error, snag:snag()})
) -> flag(GYJ).
new_builder(Name, Valuer, Getter, P) ->
    {flag, Name, <<""/utf8>>, P, Valuer, Getter, none}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 904).
-spec build_flag(flag(any())) -> flag_entry().
build_flag(Fb) ->
    {flag_entry,
        (erlang:element(5, Fb))(
            {flag_internals, erlang:element(7, Fb), erlang:element(4, Fb)}
        ),
        erlang:element(3, Fb)}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 957).
-spec attempt(
    {ok, GZB} | {error, GZC},
    fun((GZB) -> {ok, any()} | {error, GZC})
) -> {ok, GZB} | {error, GZC}.
attempt(Val, F) ->
    gleam@result:'try'(Val, fun(A) -> gleam@result:replace(F(A), A) end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 950).
-spec wrap_with_constraint(
    fun((binary()) -> {ok, GYV} | {error, snag:snag()}),
    fun((GYV) -> {ok, GYV} | {error, snag:snag()})
) -> fun((binary()) -> {ok, GYV} | {error, snag:snag()}).
wrap_with_constraint(P, Constraint) ->
    fun(Input) -> attempt(P(Input), Constraint) end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 941).
-spec flag_constraint(flag(GYR), fun((GYR) -> {ok, GYR} | {error, snag:snag()})) -> flag(GYR).
flag_constraint(Builder, Constraint) ->
    erlang:setelement(
        4,
        Builder,
        wrap_with_constraint(erlang:element(4, Builder), Constraint)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 986).
-spec flag_help(flag(GZK), binary()) -> flag(GZK).
flag_help(Flag, Description) ->
    erlang:setelement(3, Flag, Description).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 999).
-spec flag_default(flag(GZN), GZN) -> flag(GZN).
flag_default(Flag, Default) ->
    erlang:setelement(7, Flag, {some, Default}).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1009).
-spec insert(flags(), binary(), flag_entry()) -> flags().
insert(Flags, Name, Flag) ->
    {flags, gleam@dict:insert(erlang:element(2, Flags), Name, Flag)}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 410).
-spec flag(
    flag(GVX),
    fun((fun((flags()) -> {ok, GVX} | {error, snag:snag()})) -> command(GWA))
) -> command(GWA).
flag(Flag, F) ->
    Cmd = F(
        fun(_capture) ->
            (erlang:element(6, Flag))(_capture, erlang:element(2, Flag))
        end
    ),
    erlang:setelement(
        3,
        Cmd,
        insert(
            erlang:element(3, Cmd),
            erlang:element(2, Flag),
            build_flag(Flag)
        )
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1013).
-spec merge(flags(), flags()) -> flags().
merge(A, B) ->
    {flags, gleam@dict:merge(erlang:element(2, A), erlang:element(2, B))}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1017).
-spec fold(flags(), GZQ, fun((GZQ, binary(), flag_entry()) -> GZQ)) -> GZQ.
fold(Flags, Acc, F) ->
    gleam@dict:fold(erlang:element(2, Flags), Acc, F).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 724).
-spec build_flags_help(flags()) -> list(glint@internal@help:flag()).
build_flags_help(Flags) ->
    fold(
        Flags,
        [],
        fun(Acc, Name, Flag) ->
            [{flag,
                    {metadata, Name, erlang:element(3, Flag)},
                    flag_type_info(Flag)} |
                Acc]
        end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 680).
-spec build_command_help(binary(), command_node(any())) -> glint@internal@help:command().
build_command_help(Name, Node) ->
    {Description, Flags, Unnamed_args, Named_args} = begin
        _pipe = erlang:element(2, Node),
        _pipe@1 = gleam@option:map(
            _pipe,
            fun(Cmd) ->
                {erlang:element(5, Node),
                    build_flags_help(
                        merge(erlang:element(4, Node), erlang:element(3, Cmd))
                    ),
                    erlang:element(4, Cmd),
                    erlang:element(5, Cmd)}
            end
        ),
        gleam@option:unwrap(_pipe@1, {erlang:element(5, Node), [], none, []})
    end,
    {command,
        {metadata, Name, Description},
        Flags,
        build_subcommands_help(erlang:element(3, Node)),
        (gleam@option:map(Unnamed_args, fun(Args) -> case Args of
                    {eq_args, N} ->
                        {eq_args, N};

                    {min_args, N@1} ->
                        {min_args, N@1}
                end end)),
        Named_args}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1021).
-spec new_flags() -> flags().
new_flags() ->
    {flags, gleam@dict:new()}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 281).
-spec empty_command() -> command_node(any()).
empty_command() ->
    {command_node, none, gleam@dict:new(), new_flags(), <<""/utf8>>}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 311).
-spec command(fun((named_args(), list(binary()), flags()) -> GVL)) -> command(GVL).
command(Runner) ->
    {command, Runner, new_flags(), <<""/utf8>>, none, []}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1072).
-spec access_type_error(binary()) -> {ok, any()} | {error, snag:snag()}.
access_type_error(Flag_type) ->
    snag:error(<<"cannot access flag as "/utf8, Flag_type/binary>>).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1076).
-spec flag_not_provided_error() -> {ok, any()} | {error, snag:snag()}.
flag_not_provided_error() ->
    snag:error(<<"no value provided"/utf8>>).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1080).
-spec construct_value(
    binary(),
    flag_internals(GZX),
    fun((flag_internals(GZX)) -> value())
) -> {ok, value()} | {error, snag:snag()}.
construct_value(Input, Internal, Constructor) ->
    gleam@result:map(
        (erlang:element(3, Internal))(Input),
        fun(Val) -> Constructor(erlang:setelement(2, Internal, {some, Val})) end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1091).
-spec compute_flag(binary(), value()) -> {ok, value()} | {error, snag:snag()}.
compute_flag(Input, Current) ->
    _pipe = Input,
    _pipe@1 = case Current of
        {i, Internal} ->
            fun(_capture) ->
                construct_value(
                    _capture,
                    Internal,
                    fun(Field@0) -> {i, Field@0} end
                )
            end;

        {li, Internal@1} ->
            fun(_capture@1) ->
                construct_value(
                    _capture@1,
                    Internal@1,
                    fun(Field@0) -> {li, Field@0} end
                )
            end;

        {f, Internal@2} ->
            fun(_capture@2) ->
                construct_value(
                    _capture@2,
                    Internal@2,
                    fun(Field@0) -> {f, Field@0} end
                )
            end;

        {lf, Internal@3} ->
            fun(_capture@3) ->
                construct_value(
                    _capture@3,
                    Internal@3,
                    fun(Field@0) -> {lf, Field@0} end
                )
            end;

        {s, Internal@4} ->
            fun(_capture@4) ->
                construct_value(
                    _capture@4,
                    Internal@4,
                    fun(Field@0) -> {s, Field@0} end
                )
            end;

        {ls, Internal@5} ->
            fun(_capture@5) ->
                construct_value(
                    _capture@5,
                    Internal@5,
                    fun(Field@0) -> {ls, Field@0} end
                )
            end;

        {b, Internal@6} ->
            fun(_capture@6) ->
                construct_value(
                    _capture@6,
                    Internal@6,
                    fun(Field@0) -> {b, Field@0} end
                )
            end
    end(_pipe),
    snag:context(_pipe@1, <<"failed to compute value for flag"/utf8>>).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1106).
-spec layer_invalid_flag(snag:snag(), binary()) -> snag:snag().
layer_invalid_flag(Err, Flag) ->
    snag:layer(Err, <<<<"invalid flag '"/utf8, Flag/binary>>/binary, "'"/utf8>>).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1110).
-spec no_value_flag_err(binary()) -> snag:snag().
no_value_flag_err(Flag_input) ->
    _pipe = (<<<<"flag '"/utf8, Flag_input/binary>>/binary,
        "' has no assigned value"/utf8>>),
    _pipe@1 = snag:new(_pipe),
    layer_invalid_flag(_pipe@1, Flag_input).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1116).
-spec undefined_flag_err(binary()) -> snag:snag().
undefined_flag_err(Key) ->
    _pipe = <<"flag provided but not defined"/utf8>>,
    _pipe@1 = snag:new(_pipe),
    layer_invalid_flag(_pipe@1, Key).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1122).
-spec cannot_parse(binary(), binary()) -> snag:snag().
cannot_parse(Value, Kind) ->
    _pipe = (<<<<<<"cannot parse value '"/utf8, Value/binary>>/binary,
            "' as "/utf8>>/binary,
        Kind/binary>>),
    snag:new(_pipe).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1131).
-spec get(flags(), binary()) -> {ok, flag_entry()} | {error, snag:snag()}.
get(Flags, Name) ->
    _pipe = gleam@dict:get(erlang:element(2, Flags), Name),
    gleam@result:replace_error(_pipe, undefined_flag_err(Name)).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1038).
-spec update_flag_value(flags(), {binary(), binary()}) -> {ok, flags()} |
    {error, snag:snag()}.
update_flag_value(Flags, Data) ->
    {Key, Input} = Data,
    gleam@result:'try'(
        get(Flags, Key),
        fun(Contents) ->
            gleam@result:map(
                begin
                    _pipe = compute_flag(Input, erlang:element(2, Contents)),
                    gleam@result:map_error(
                        _pipe,
                        fun(_capture) -> layer_invalid_flag(_capture, Key) end
                    )
                end,
                fun(Value) ->
                    insert(Flags, Key, erlang:setelement(2, Contents, Value))
                end
            )
        end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1051).
-spec attempt_toggle_flag(flags(), binary()) -> {ok, flags()} |
    {error, snag:snag()}.
attempt_toggle_flag(Flags, Key) ->
    gleam@result:'try'(
        get(Flags, Key),
        fun(Contents) -> case erlang:element(2, Contents) of
                {b, {flag_internals, none, _} = Internal} ->
                    _pipe = erlang:setelement(2, Internal, {some, true}),
                    _pipe@1 = {b, _pipe},
                    _pipe@2 = (fun(Val) ->
                        erlang:setelement(2, Contents, Val)
                    end)(_pipe@1),
                    _pipe@3 = gleam@dict:insert(
                        erlang:element(2, Flags),
                        Key,
                        _pipe@2
                    ),
                    _pipe@4 = {flags, _pipe@3},
                    {ok, _pipe@4};

                {b, {flag_internals, {some, Val@1}, _} = Internal@1} ->
                    _pipe@5 = erlang:setelement(
                        2,
                        Internal@1,
                        {some, not Val@1}
                    ),
                    _pipe@6 = {b, _pipe@5},
                    _pipe@7 = (fun(Val@2) ->
                        erlang:setelement(2, Contents, Val@2)
                    end)(_pipe@6),
                    _pipe@8 = gleam@dict:insert(
                        erlang:element(2, Flags),
                        Key,
                        _pipe@7
                    ),
                    _pipe@9 = {flags, _pipe@8},
                    {ok, _pipe@9};

                _ ->
                    {error, no_value_flag_err(Key)}
            end end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1136).
-spec get_value(
    flags(),
    binary(),
    fun((flag_entry()) -> {ok, HAD} | {error, snag:snag()})
) -> {ok, HAD} | {error, snag:snag()}.
get_value(Flags, Key, Kind) ->
    _pipe = get(Flags, Key),
    _pipe@1 = gleam@result:'try'(_pipe, Kind),
    snag:context(
        _pipe@1,
        <<<<"failed to retrieve value for flag '"/utf8, Key/binary>>/binary,
            "'"/utf8>>
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1151).
-spec get_flag(flags(), flag(HAG)) -> {ok, HAG} | {error, snag:snag()}.
get_flag(Flags, Flag) ->
    (erlang:element(6, Flag))(Flags, erlang:element(2, Flag)).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1157).
-spec get_int_flag(flags(), binary()) -> {ok, integer()} | {error, snag:snag()}.
get_int_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {i, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {i, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"int"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 822).
-spec int_flag(binary()) -> flag(integer()).
int_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {i, Field@0} end,
        fun get_int_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@int:parse(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                cannot_parse(Input, <<"int"/utf8>>)
            ) end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1168).
-spec get_ints_flag(flags(), binary()) -> {ok, list(integer())} |
    {error, snag:snag()}.
get_ints_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {li, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {li, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"int list"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 831).
-spec ints_flag(binary()) -> flag(list(integer())).
ints_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {li, Field@0} end,
        fun get_ints_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            _pipe@2 = gleam@list:try_map(_pipe@1, fun gleam@int:parse/1),
            gleam@result:replace_error(
                _pipe@2,
                cannot_parse(Input, <<"int list"/utf8>>)
            ) end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1179).
-spec get_bool_flag(flags(), binary()) -> {ok, boolean()} | {error, snag:snag()}.
get_bool_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {b, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {b, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"bool"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 875).
-spec bool_flag(binary()) -> flag(boolean()).
bool_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {b, Field@0} end,
        fun get_bool_flag/2,
        fun(Input) -> case gleam@string:lowercase(Input) of
                <<"true"/utf8>> ->
                    {ok, true};

                <<"t"/utf8>> ->
                    {ok, true};

                <<"false"/utf8>> ->
                    {ok, false};

                <<"f"/utf8>> ->
                    {ok, false};

                _ ->
                    {error, cannot_parse(Input, <<"bool"/utf8>>)}
            end end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1190).
-spec get_string_flag(flags(), binary()) -> {ok, binary()} |
    {error, snag:snag()}.
get_string_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {s, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {s, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"string"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 860).
-spec string_flag(binary()) -> flag(binary()).
string_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {s, Field@0} end,
        fun get_string_flag/2,
        fun(S) -> {ok, S} end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1201).
-spec get_strings_flag(flags(), binary()) -> {ok, list(binary())} |
    {error, snag:snag()}.
get_strings_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {ls, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {ls, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"string list"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 866).
-spec strings_flag(binary()) -> flag(list(binary())).
strings_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {ls, Field@0} end,
        fun get_strings_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            {ok, _pipe@1} end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1215).
-spec get_floats(flags(), binary()) -> {ok, float()} | {error, snag:snag()}.
get_floats(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {f, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {f, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"float"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 841).
-spec float_flag(binary()) -> flag(float()).
float_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {f, Field@0} end,
        fun get_floats/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@float:parse(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                cannot_parse(Input, <<"float"/utf8>>)
            ) end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1226).
-spec get_floats_flag(flags(), binary()) -> {ok, list(float())} |
    {error, snag:snag()}.
get_floats_flag(Flags, Name) ->
    get_value(Flags, Name, fun(Flag) -> case erlang:element(2, Flag) of
                {lf, {flag_internals, {some, Val}, _}} ->
                    {ok, Val};

                {lf, {flag_internals, none, _}} ->
                    flag_not_provided_error();

                _ ->
                    access_type_error(<<"float list"/utf8>>)
            end end).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 850).
-spec floats_flag(binary()) -> flag(list(float())).
floats_flag(Name) ->
    new_builder(
        Name,
        fun(Field@0) -> {lf, Field@0} end,
        fun get_floats_flag/2,
        fun(Input) -> _pipe = Input,
            _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
            _pipe@2 = gleam@list:try_map(_pipe@1, fun gleam@float:parse/1),
            gleam@result:replace_error(
                _pipe@2,
                cannot_parse(Input, <<"float list"/utf8>>)
            ) end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1252).
-spec do_update_at(
    command_node(HAZ),
    list(binary()),
    fun((command_node(HAZ)) -> command_node(HAZ))
) -> command_node(HAZ).
do_update_at(Node, Path, F) ->
    case Path of
        [] ->
            F(Node);

        [Next | Rest] ->
            erlang:setelement(
                3,
                Node,
                (gleam@dict:upsert(
                    erlang:element(3, Node),
                    Next,
                    fun(Found) -> _pipe = Found,
                        _pipe@1 = gleam@option:lazy_unwrap(
                            _pipe,
                            fun empty_command/0
                        ),
                        do_update_at(_pipe@1, Rest, F) end
                ))
            )
    end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1241).
-spec update_at(
    glint(HAT),
    list(binary()),
    fun((command_node(HAT)) -> command_node(HAT))
) -> glint(HAT).
update_at(Glint, Path, F) ->
    erlang:setelement(
        3,
        Glint,
        do_update_at(erlang:element(3, Glint), sanitize_path(Path), F)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 223).
-spec path_help(glint(GUV), list(binary()), binary()) -> glint(GUV).
path_help(Glint, Path, Description) ->
    update_at(
        Glint,
        Path,
        fun(Node) -> erlang:setelement(5, Node, Description) end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 261).
-spec add(glint(GVC), list(binary()), command(GVC)) -> glint(GVC).
add(Glint, Path, Command) ->
    update_at(
        Glint,
        Path,
        fun(Node) ->
            erlang:setelement(
                2,
                erlang:setelement(5, Node, erlang:element(4, Command)),
                {some,
                    {internal_command,
                        erlang:element(2, Command),
                        erlang:element(3, Command),
                        erlang:element(5, Command),
                        erlang:element(6, Command)}}
            )
        end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 421).
-spec group_flag(glint(GWD), list(binary()), flag(any())) -> glint(GWD).
group_flag(Glint, Path, Flag) ->
    update_at(
        Glint,
        Path,
        fun(Node) ->
            erlang:setelement(
                4,
                Node,
                insert(
                    erlang:element(4, Node),
                    erlang:element(2, Flag),
                    build_flag(Flag)
                )
            )
        end
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 213).
-spec new() -> glint(any()).
new() ->
    {glint,
        {config, none, none, false, none, true, 4, 80, 20, 2},
        empty_command()}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 661).
-spec build_help_config(config()) -> glint@internal@help:config().
build_help_config(Config) ->
    {config,
        erlang:element(3, Config),
        gleam@option:map(
            erlang:element(2, Config),
            fun(P) -> erlang:element(2, P) end
        ),
        gleam@option:map(
            erlang:element(2, Config),
            fun(P@1) -> erlang:element(3, P@1) end
        ),
        gleam@option:map(
            erlang:element(2, Config),
            fun(P@2) -> erlang:element(4, P@2) end
        ),
        erlang:element(4, Config),
        erlang:element(5, Config),
        erlang:element(7, Config),
        erlang:element(8, Config),
        erlang:element(9, Config),
        erlang:element(10, Config),
        <<"--"/utf8>>,
        <<"="/utf8>>}.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 650).
-spec cmd_help(list(binary()), command_node(any()), config()) -> binary().
cmd_help(Path, Cmd, Config) ->
    _pipe = Path,
    _pipe@1 = lists:reverse(_pipe),
    _pipe@2 = gleam@string:join(_pipe@1, <<" "/utf8>>),
    _pipe@3 = build_command_help(_pipe@2, Cmd),
    glint@internal@help:command_help_to_string(
        _pipe@3,
        build_help_config(Config)
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 1029).
-spec update_flags(flags(), binary()) -> {ok, flags()} | {error, snag:snag()}.
update_flags(Flags, Flag_input) ->
    Flag_input@1 = gleam@string:drop_left(
        Flag_input,
        gleam@string:length(<<"--"/utf8>>)
    ),
    case gleam@string:split_once(Flag_input@1, <<"="/utf8>>) of
        {ok, Data} ->
            update_flag_value(Flags, Data);

        {error, _} ->
            attempt_toggle_flag(Flags, Flag_input@1)
    end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 529).
-spec execute_root(
    list(binary()),
    config(),
    command_node(GWZ),
    list(binary()),
    list(binary())
) -> {ok, out(GWZ)} | {error, binary()}.
execute_root(Path, Config, Cmd, Args, Flag_inputs) ->
    Res = begin
        _pipe@5 = (gleam@option:map(
            erlang:element(2, Cmd),
            fun(Contents) ->
                gleam@result:'try'(
                    gleam@list:try_fold(
                        Flag_inputs,
                        merge(
                            erlang:element(4, Cmd),
                            erlang:element(3, Contents)
                        ),
                        fun update_flags/2
                    ),
                    fun(New_flags) ->
                        gleam@result:'try'(
                            begin
                                Named = gleam@list:zip(
                                    erlang:element(5, Contents),
                                    Args
                                ),
                                case erlang:length(Named) =:= erlang:length(
                                    erlang:element(5, Contents)
                                ) of
                                    true ->
                                        {ok, maps:from_list(Named)};

                                    false ->
                                        snag:error(
                                            <<"unmatched named arguments: "/utf8,
                                                (begin
                                                    _pipe = erlang:element(
                                                        5,
                                                        Contents
                                                    ),
                                                    _pipe@1 = gleam@list:drop(
                                                        _pipe,
                                                        erlang:length(Named)
                                                    ),
                                                    _pipe@2 = gleam@list:map(
                                                        _pipe@1,
                                                        fun(S) ->
                                                            <<<<"'"/utf8,
                                                                    S/binary>>/binary,
                                                                "'"/utf8>>
                                                        end
                                                    ),
                                                    gleam@string:join(
                                                        _pipe@2,
                                                        <<", "/utf8>>
                                                    )
                                                end)/binary>>
                                        )
                                end
                            end,
                            fun(Named_args) ->
                                Args@1 = gleam@list:drop(
                                    Args,
                                    maps:size(Named_args)
                                ),
                                gleam@result:map(
                                    case erlang:element(4, Contents) of
                                        {some, Count} ->
                                            _pipe@3 = Count,
                                            _pipe@4 = args_compare(
                                                _pipe@3,
                                                erlang:length(Args@1)
                                            ),
                                            snag:context(
                                                _pipe@4,
                                                <<"invalid number of arguments provided"/utf8>>
                                            );

                                        none ->
                                            {ok, nil}
                                    end,
                                    fun(_) ->
                                        {out,
                                            (erlang:element(2, Contents))(
                                                {named_args, Named_args},
                                                Args@1,
                                                New_flags
                                            )}
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )),
        _pipe@6 = gleam@option:unwrap(
            _pipe@5,
            snag:error(<<"command not found"/utf8>>)
        ),
        _pipe@7 = snag:context(_pipe@6, <<"failed to run command"/utf8>>),
        gleam@result:map_error(
            _pipe@7,
            fun(Err) -> {Err, cmd_help(Path, Cmd, Config)} end
        )
    end,
    case Res of
        {ok, Out} ->
            {ok, Out};

        {error, {Snag, Help}} ->
            {error,
                <<<<(snag:pretty_print(Snag))/binary,
                        "\nSee the following help text, available via the '--help' flag.\n\n"/utf8>>/binary,
                    Help/binary>>}
    end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 464).
-spec do_execute(
    command_node(GWP),
    config(),
    list(binary()),
    list(binary()),
    boolean(),
    list(binary())
) -> {ok, out(GWP)} | {error, binary()}.
do_execute(Cmd, Config, Args, Flags, Help, Command_path) ->
    case Args of
        [] when Help ->
            _pipe = Command_path,
            _pipe@1 = cmd_help(_pipe, Cmd, Config),
            _pipe@2 = {help, _pipe@1},
            {ok, _pipe@2};

        [] ->
            execute_root(Command_path, Config, Cmd, [], Flags);

        [Arg | Rest] ->
            case gleam@dict:get(erlang:element(3, Cmd), Arg) of
                {ok, Sub_command} ->
                    Sub_command@1 = erlang:setelement(
                        4,
                        Sub_command,
                        merge(
                            erlang:element(4, Cmd),
                            erlang:element(4, Sub_command)
                        )
                    ),
                    do_execute(
                        Sub_command@1,
                        Config,
                        Rest,
                        Flags,
                        Help,
                        [Arg | Command_path]
                    );

                _ when Help ->
                    _pipe@3 = Command_path,
                    _pipe@4 = cmd_help(_pipe@3, Cmd, Config),
                    _pipe@5 = {help, _pipe@4},
                    {ok, _pipe@5};

                _ ->
                    execute_root(Command_path, Config, Cmd, Args, Flags)
            end
    end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 445).
-spec execute(glint(GWJ), list(binary())) -> {ok, out(GWJ)} | {error, binary()}.
execute(Glint, Args) ->
    Help_flag = <<"--"/utf8,
        (erlang:element(
            2,
            erlang:element(
                2,
                {flag,
                    {metadata,
                        <<"help"/utf8>>,
                        <<"Print help information"/utf8>>},
                    <<""/utf8>>}
            )
        ))/binary>>,
    {Help, Args@2} = case gleam@list:pop(Args, fun(S) -> S =:= Help_flag end) of
        {ok, {_, Args@1}} ->
            {true, Args@1};

        _ ->
            {false, Args}
    end,
    {Flags, Args@3} = gleam@list:partition(
        Args@2,
        fun(_capture) -> gleam@string:starts_with(_capture, <<"--"/utf8>>) end
    ),
    do_execute(
        erlang:element(3, Glint),
        erlang:element(2, Glint),
        Args@3,
        Flags,
        Help,
        []
    ).

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 606).
-spec run_and_handle(glint(GXJ), list(binary()), fun((GXJ) -> any())) -> nil.
run_and_handle(Glint, Args, Handle) ->
    case execute(Glint, Args) of
        {error, S} ->
            gleam@io:println(S),
            case erlang:element(6, erlang:element(2, Glint)) of
                true ->
                    erlang:halt(1);

                false ->
                    nil
            end;

        {ok, {help, S@1}} ->
            gleam@io:println(S@1);

        {ok, {out, Out}} ->
            Handle(Out),
            nil
    end.

-file("/home/eli/dev/reference/build/packages/glint/src/glint.gleam", 596).
-spec run(glint(any()), list(binary())) -> nil.
run(Glint, Args) ->
    run_and_handle(Glint, Args, fun(_) -> nil end).

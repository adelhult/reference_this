-module(reference).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([reference_/0]).

-type reference_() :: {reference, list(binary()), binary(), binary()}.

-file("/home/eli/dev/reference/src/reference.gleam", 23).
-spec path_from_string(binary()) -> list(binary()).
path_from_string(S) ->
    gleam@string:split(S, <<"/"/utf8>>).

-file("/home/eli/dev/reference/src/reference.gleam", 28).
-spec is_gleam_file(binary()) -> boolean().
is_gleam_file(Path) ->
    Path@1 = path_from_string(Path),
    _pipe = (gleam@result:'try'(
        gleam@list:last(Path@1),
        fun(Filename) ->
            Parts = gleam@string:split(Filename, <<"."/utf8>>),
            gleam@result:'try'(
                gleam@list:last(Parts),
                fun(Extension) -> {ok, Extension =:= <<"gleam"/utf8>>} end
            )
        end
    )),
    gleam@result:unwrap(_pipe, false).

-file("/home/eli/dev/reference/src/reference.gleam", 50).
-spec escape_helper(list(binary()), gleam@string_builder:string_builder()) -> binary().
escape_helper(Chars, Sb) ->
    case Chars of
        [] ->
            gleam@string_builder:to_string(Sb);

        [C | Cs] ->
            case C of
                <<"\\"/utf8>> ->
                    escape_helper(
                        Cs,
                        gleam@string_builder:append(Sb, <<"\\\\"/utf8>>)
                    );

                <<"\n"/utf8>> ->
                    escape_helper(
                        Cs,
                        gleam@string_builder:append(Sb, <<"\\n"/utf8>>)
                    );

                <<"\""/utf8>> ->
                    escape_helper(
                        Cs,
                        gleam@string_builder:append(Sb, <<"\\\""/utf8>>)
                    );

                C@1 ->
                    escape_helper(Cs, gleam@string_builder:append(Sb, C@1))
            end
    end.

-file("/home/eli/dev/reference/src/reference.gleam", 64).
-spec escape(binary()) -> binary().
escape(S) ->
    escape_helper(gleam@string:to_graphemes(S), gleam@string_builder:new()).

-file("/home/eli/dev/reference/src/reference.gleam", 68).
-spec qoute(binary()) -> binary().
qoute(S) ->
    S@1 = escape(S),
    <<<<"\""/utf8, S@1/binary>>/binary, "\""/utf8>>.

-file("/home/eli/dev/reference/src/reference.gleam", 73).
-spec serialize_path(list(binary())) -> binary().
serialize_path(Path) ->
    <<<<"["/utf8,
            (begin
                _pipe = Path,
                _pipe@1 = gleam@list:map(_pipe, fun qoute/1),
                gleam@string:join(_pipe@1, <<", "/utf8>>)
            end)/binary>>/binary,
        "]"/utf8>>.

-file("/home/eli/dev/reference/src/reference.gleam", 77).
-spec serialize_reference(reference_()) -> binary().
serialize_reference(Reference) ->
    {reference, Path, Module_doc, Content} = Reference,
    <<<<<<<<<<<<<<<<<<"Reference("/utf8, "path: "/utf8>>/binary,
                                    (serialize_path(Path))/binary>>/binary,
                                ", "/utf8>>/binary,
                            "module_doc: "/utf8>>/binary,
                        (qoute(Module_doc))/binary>>/binary,
                    ", "/utf8>>/binary,
                "content: "/utf8>>/binary,
            (qoute(Content))/binary>>/binary,
        ")"/utf8>>.

-file("/home/eli/dev/reference/src/reference.gleam", 91).
-spec serialize_reference_list(list(reference_())) -> binary().
serialize_reference_list(References) ->
    Indent = gleam@string:repeat(<<" "/utf8>>, 2),
    References@1 = begin
        _pipe = References,
        _pipe@1 = gleam@list:map(_pipe, fun serialize_reference/1),
        gleam@string:join(_pipe@1, <<",\n"/utf8, Indent/binary>>)
    end,
    <<<<<<"pub const references: List(Reference) = [\n"/utf8, Indent/binary>>/binary,
            References@1/binary>>/binary,
        "\n]"/utf8>>.

-file("/home/eli/dev/reference/src/reference.gleam", 102).
-spec consume_module_doc(binary()) -> {binary(), binary()}.
consume_module_doc(Content) ->
    Lines = gleam@string:split(Content, <<"\n"/utf8>>),
    Prefixed_slashes = fun(S) ->
        gleam@string:starts_with(S, <<"////"/utf8>>)
    end,
    {Doc, Rest} = gleam@list:split_while(Lines, Prefixed_slashes),
    Doc@1 = gleam@list:fold(
        Doc,
        <<""/utf8>>,
        fun(Result, Line) ->
            _assert_subject = gleam@string:split_once(Line, <<"////"/utf8>>),
            {ok, {_, Stripped_line}} = case _assert_subject of
                {ok, {_, _}} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"reference"/utf8>>,
                                function => <<"consume_module_doc"/utf8>>,
                                line => 112})
            end,
            <<<<Result/binary, Stripped_line/binary>>/binary, "\n"/utf8>>
        end
    ),
    Rest@1 = begin
        _pipe = Rest,
        gleam@string:join(_pipe, <<"\n"/utf8>>)
    end,
    {Doc@1, Rest@1}.

-file("/home/eli/dev/reference/src/reference.gleam", 142).
-spec run(binary(), binary()) -> {ok, nil} | {error, simplifile:file_error()}.
run(Directory, Output_file) ->
    gleam@result:'try'(
        simplifile:get_files(Directory),
        fun(Files) ->
            Files@1 = gleam@list:filter(Files, fun is_gleam_file/1),
            gleam@result:'try'(
                begin
                    _pipe = Files@1,
                    _pipe@1 = gleam@list:map(_pipe, fun simplifile:read/1),
                    gleam@result:all(_pipe@1)
                end,
                fun(Files_content) ->
                    Files_content@1 = begin
                        _pipe@2 = Files_content,
                        gleam@list:map(_pipe@2, fun consume_module_doc/1)
                    end,
                    References = begin
                        _pipe@3 = gleam@list:zip(Files@1, Files_content@1),
                        gleam@list:map(
                            _pipe@3,
                            fun(File) ->
                                {Path, {Doc, Content}} = File,
                                Path@1 = path_from_string(Path),
                                {reference, Path@1, Doc, Content}
                            end
                        )
                    end,
                    Output_file_content = <<<<(<<"//// This module has been generated by the 'reference' tool.\n\n"/utf8,
                                "pub type Path = List(String)\n\n"/utf8,
                                "pub type Reference {\n"/utf8,
                                "  Reference(path: Path, module_doc: String, content: String)\n"/utf8,
                                "}\n"/utf8>>)/binary,
                            "\n"/utf8>>/binary,
                        (serialize_reference_list(References))/binary>>,
                    simplifile:write(Output_file, Output_file_content)
                end
            )
        end
    ).

-file("/home/eli/dev/reference/src/reference.gleam", 121).
-spec run_cli() -> glint:command(nil).
run_cli() ->
    glint:command_help(
        <<"Reads all gleam files in <INPUT_DIRECTORY> and saves as strings in the gleam file <OUTPUT_FILE>."/utf8>>,
        fun() ->
            glint:named_arg(
                <<"INPUT_DIRECTORY"/utf8>>,
                fun(Directory_handle) ->
                    glint:named_arg(
                        <<"OUTPUT_FILE"/utf8>>,
                        fun(Output_file_handle) ->
                            glint:command(
                                fun(Named, _, _) ->
                                    Directory = Directory_handle(Named),
                                    Output_file = Output_file_handle(Named),
                                    case run(Directory, Output_file) of
                                        {error, Error} ->
                                            gleam@io:println_error(
                                                <<"Error: "/utf8,
                                                    (simplifile:describe_error(
                                                        Error
                                                    ))/binary>>
                                            );

                                        _ ->
                                            nil
                                    end
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("/home/eli/dev/reference/src/reference.gleam", 169).
-spec main() -> nil.
main() ->
    _pipe = glint:new(),
    _pipe@1 = glint:with_name(_pipe, <<"Reference"/utf8>>),
    _pipe@2 = glint:pretty_help(_pipe@1, glint:default_pretty_help()),
    _pipe@3 = glint:add(_pipe@2, [], run_cli()),
    glint:run(_pipe@3, erlang:element(4, argv:load())).

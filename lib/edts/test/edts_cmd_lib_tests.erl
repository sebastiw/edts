-module(edts_cmd_lib_tests).

-include_lib("eunit/include/eunit.hrl").

arity_validate_test_() ->
    CtxF = fun(A) -> orddict:from_list([{arity, A}]) end,
    [{"Not set at all",
      ?_assertEqual({error, {arity, notfound}},
                    edts_cmd_lib:validate(orddict:new(), [arity]))},
     {"Arity zero",
      ?_assertEqual({ok, [{arity, 0}]},
                    edts_cmd_lib:validate(CtxF("0"), [arity]))},
     {"Arity one",
      ?_assertEqual({ok, [{arity, 1}]},
                    edts_cmd_lib:validate(CtxF("1"), [arity]))},
     {"Negative arity",
      ?_assertEqual({error, {arity, {badarg, "-1"}}},
                    edts_cmd_lib:validate(CtxF("-1"), [arity]))},
     {"Illegal arity",
      ?_assertEqual({error, {arity, {badarg,"a"}}},
                    edts_cmd_lib:validate(CtxF("a"), [arity]))}
    ].

string_validate_test_() ->
    CtxF = fun(Var, Val) -> orddict:from_list([{Var, Val}]) end,
    [[{"Not set at all",
       ?_assertEqual({error, {Var, notfound}},
                     edts_cmd_lib:validate(orddict:new(), [Var]))},
      {"Normal flow",
       ?_assertEqual({ok, [{Var, "a_string"}]},
                     edts_cmd_lib:validate(CtxF(Var, "a_string"), [Var]))}
     ]
     || Var <- [code, string]
    ].

string_validate_with_default_test_() ->
    CtxF = fun(Var, Val) -> orddict:from_list([{Var, Val}]) end,
    [[{"Not set at all",
       ?_assertEqual({ok, [{Var, Default}]},
                     edts_cmd_lib:validate(orddict:new(), [Var]))},
      {"Normal flow",
       ?_assertEqual({ok, [{Var, "a_string"}]},
                     edts_cmd_lib:validate(CtxF(Var, "a_string"), [Var]))}
     ]
     || {Var, Default} <- [{erlang_cookie, undefined}, {project_name, ""}]
    ].

strings_validate_test_() ->
    CtxF = fun(Val) -> orddict:from_list([{expressions, Val}]) end,
    [{"Not set at all",
      ?_assertEqual({error, {expressions, notfound}},
                    edts_cmd_lib:validate(orddict:new(), [expressions]))},
     {"Normal flow",
      ?_assertEqual({ok, [{expressions, ["a_string", "a_string"]}]},
                    edts_cmd_lib:validate(CtxF(["a_string", "a_string"]), [expressions]))},
     {"Not string lists",
      ?_assertEqual({error, {expressions, {not_strings, "a_string"}}},
                    edts_cmd_lib:validate(CtxF("a_string"), [expressions]))},
     {"Not lists at all",
      ?_assertEqual({error, {expressions, {not_list, 34}}},
                    edts_cmd_lib:validate(CtxF(34), [expressions]))}
    ].

non_neg_integer_validate_test_() ->
    CtxF = fun(Var, Val) -> orddict:from_list([{Var, Val}]) end,
    [[{"Not set at all",
       ?_assertEqual({error, {Var, notfound}},
                     edts_cmd_lib:validate(orddict:new(), [Var]))},
      {"Positive zero",
       ?_assertEqual({ok, [{Var, 0}]},
                     edts_cmd_lib:validate(CtxF(Var, "0"), [Var]))},
      {"Negative zero",
       ?_assertEqual({ok, [{Var, 0}]},
                     edts_cmd_lib:validate(CtxF(Var, "-0"), [Var]))},
      {"Positive one",
       ?_assertEqual({ok, [{Var, 1}]},
                     edts_cmd_lib:validate(CtxF(Var, "1"), [Var]))},
      {"Negative one - fail",
       ?_assertEqual({error, {Var, {illegal, "-1"}}},
                     edts_cmd_lib:validate(CtxF(Var, "-1"), [Var]))},
      {"Illegal value - fail",
       ?_assertEqual({error, {Var, {badarg, "a"}}},
                     edts_cmd_lib:validate(CtxF(Var, "a"), [Var]))}
     ]
     || Var <- [indent, max_column]
    ].

file_validate_test_() ->
    CtxF = fun(A) -> orddict:from_list([{file, A}]) end,
    {ok, Cwd} = file:get_cwd(),
    Filename = filename:join(Cwd, "asotehu"),
    [{"Not set at all",
      ?_assertEqual({error, {file, notfound}}, edts_cmd_lib:validate(orddict:new(), [file]))},
     {"File exist",
      ?_assertEqual({ok, [{file, Cwd}]}, edts_cmd_lib:validate(CtxF(Cwd), [file]))},
     {"File does not exist",
      ?_assertEqual({error, {file, {no_exists, Filename}}}, edts_cmd_lib:validate(CtxF(Filename), [file]))}
    ].

function_validate_test_() ->
    CtxF = fun (A) -> orddict:from_list([{function, A}]) end,
    [{"Not set at all",
      ?_assertEqual({error, {function, notfound}},
                    edts_cmd_lib:validate(orddict:new(), [function]))},
     {"Normal flow",
      ?_assertEqual({ok, [{function, foo}]},
                    edts_cmd_lib:validate(CtxF("foo"), [function]))}
    ].

dirs_validate_validate_test_() ->
    {ok, Cwd} = file:get_cwd(),
    LibDir = filename:basename(Cwd),
    CtxF = fun(Var) -> orddict:from_list([{Var, [LibDir, LibDir]}]) end,
    [[{"Not set at all - empty dirs",
       ?_assertEqual({ok, [{Var, []}]},
                     edts_cmd_lib:validate(orddict:new(), [Var]))},
      {"Two directories",
       ?_assertEqual({ok, [{Var, [LibDir, LibDir]}]},
                     edts_cmd_lib:validate(CtxF(Var), [Var]))}
     ]
     || Var <- [project_lib_dirs, project_include_dirs, app_include_dirs]
    ].

module_validate_test_() ->
    CtxF = fun(M) -> orddict:from_list([{module, M}]) end,
    [{"Not set at all",
      ?_assertEqual({error, {module, notfound}},
                    edts_cmd_lib:validate(orddict:new(), [module]))},
     {"Normal flow",
      ?_assertEqual({ok, [{module, foo}]},
                    edts_cmd_lib:validate(CtxF("foo"), [module]))}
    ].

nodename_validate_test_() ->
    [_Name, Hostname] = string:tokens(atom_to_list(node()), "@"),
    Ctx = orddict:from_list([{nodename, "foo"}]),
    [{"Not set at all - crash",
      ?_assertError(function_clause,
                    edts_cmd_lib:validate(orddict:new(), [nodename]))},
     {"Full nodename with hostname",
      ?_assertEqual({ok, [{nodename, list_to_atom("foo@" ++ Hostname)}]},
                    edts_cmd_lib:validate(Ctx, [nodename]))}
    ].

project_root_validate_test_() ->
    {ok, Cwd} = file:get_cwd(),
    Path = filename:join(Cwd, "asotehu"),
    CtxF = fun(P) -> orddict:from_list([{project_root, P}]) end,
    [{"Not set at all - use default",
      ?_assertEqual({ok, [{project_root, ""}]},
                    edts_cmd_lib:validate(orddict:new(), [project_root]))},
     {"Existing dir",
      ?_assertEqual({ok, [{project_root, Cwd}]},
                    edts_cmd_lib:validate(CtxF(Cwd), [project_root]))},
     {"Non-existing dir",
      ?_assertEqual({error, {project_root, {not_dir, Path}}},
                    edts_cmd_lib:validate(CtxF(Path), [project_root]))}
    ].

enum_validate_test_() ->
    CtxF = fun (E) -> orddict:from_list([{info_level, E}]) end,
    [{"Not set at all - use default value",
      ?_assertEqual({ok, [{info_level, basic}]},
                    edts_cmd_lib:validate(orddict:new(), [info_level]))},
     {"Explicitly set to basic",
      ?_assertEqual({ok, [{info_level, basic}]},
                    edts_cmd_lib:validate(CtxF("basic"), [info_level]))},
     {"Explicitly set to detailed",
      ?_assertEqual({ok, [{info_level, detailed}]},
                    edts_cmd_lib:validate(CtxF("detailed"), [info_level]))},
     {"Illegal value",
      ?_assertEqual({error, {info_level, {illegal, "c"}}},
                    edts_cmd_lib:validate(CtxF("c"), [info_level]))}
     ].

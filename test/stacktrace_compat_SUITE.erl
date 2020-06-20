%% Copyright (c) 2018 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO WORK SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(stacktrace_compat_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Boilerplate
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    GroupNames = [individual_tests],
    [{GroupName, [], individual_test_cases()} || GroupName <- GroupNames].

individual_test_cases() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    [Name || {Name, 1} <- Exports, lists:suffix("_test", atom_to_list(Name))].

%% ------------------------------------------------------------------
%% Initialization
%% ------------------------------------------------------------------

init_per_group(_Name, Config) ->
    {ok, _} = application:ensure_all_started(sasl),
    {ok, _} = application:ensure_all_started(stacktrace_compat),
    Config.

end_per_group(_Name, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Definition
%% ------------------------------------------------------------------

naked_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise, naked_capture).

throw_capture_pattern_test(_Config) ->
    assert_expected_stacktraces_for(raise, throw_capture_pattern).

capture_after_variable_export_test(_Config) ->
    assert_expected_stacktraces_for(raise, capture_after_variable_export).

no_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise, no_capture).

function_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise, function_capture).

nested_function_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise, nested_function_capture).

nested_function_capture_with_both_test(_Config) ->
    assert_expected_stacktraces_for(raise, nested_function_capture_with_both).

function_capture_in_expression_test(_Config) ->
    assert_expected_stacktraces_for(raise, function_capture_in_expression).

function_capture_in_result_handler_test(_Config) ->
    assert_expected_stacktraces_for(raise, function_capture_in_result_handler).

helper_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise, helper_capture).

-ifdef(POST_OTP_20).
unused_var_with_function_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise21, unused_var_with_function_capture).

var_capture_test(_Config) ->
    assert_expected_stacktraces_for(raise21, var_capture).

nested_var_capture_with_both_test(_Config) ->
    assert_expected_stacktraces_for(raise21, nested_var_capture_with_both).
-endif.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

assert_expected_stacktraces_for(Function, CaseName) ->
    {ok, Cwd} = file:get_cwd(),
    TestModuleBeamPath = filename:join(Cwd, "test_module"),
    ct:pal("TestModuleBeamPath: ~p", [TestModuleBeamPath]),

    compile_and_load_test_module([]),
    assert_test_module_has_no_transform(),
    {CaseName, WithoutTransformST} = test_module:Function(CaseName),

    compile_and_load_test_module([{parse_transform, stacktrace_transform}]),
    assert_test_module_has_transform(),
    {CaseName, WithTransformST} = test_module:Function(CaseName),

    assert_expected_stacktraces(Function, CaseName, WithoutTransformST, WithTransformST).

compile_and_load_test_module(ExtraOptions) ->
    _ = code:purge(test_module),
    {ok, test_module, Beam} = compile_test_module(ExtraOptions),
    {module, test_module} = code:load_binary(test_module, "test_module.erl", Beam),
    ok.

compile_test_module(ExtraOptions) ->
    Options =
        [binary,
         report_errors,
         report_warnings,
         debug_info
         | case erlang:system_info(otp_release) of
               [V|_] when V >= $3, V =< $9 ->
                   [{d, 'POST_OTP_20'},
                    {d, 'POST_OTP_22'}];
               [$2,V|_] when V >= $3 ->
                   [{d, 'POST_OTP_20'},
                    {d, 'POST_OTP_22'}];
               [$2,V|_] when V >= $1 ->
                   [{d, 'POST_OTP_20'}];
               _ ->
                   []
           end
         ]
        ++ ExtraOptions,
    TestModulePath = "../../lib/stacktrace_compat/test/test_module.erl",
    compile:file(TestModulePath, Options).

assert_test_module_has_no_transform() ->
    CompileAttr = test_module:module_info(compile),
    CompileOptions = proplists:get_value(options, CompileAttr, []),
    ?assertNot(lists:member({parse_transform, stacktrace_transform}, CompileOptions)).

assert_test_module_has_transform() ->
    CompileAttr = test_module:module_info(compile),
    CompileOptions = proplists:get_value(options, CompileAttr, []),
    ?assert(lists:member({parse_transform, stacktrace_transform}, CompileOptions)).

-ifdef(POST_OTP_22).
assert_expected_stacktraces(Function, CaseName, WithoutTransformST, WithTransformST)
  when Function =:= raise, (CaseName =/= naked_capture andalso
                            CaseName =/= no_capture andalso
                            CaseName =/= helper_capture);
       Function =:= raise21, (CaseName =:= unused_var_with_function_capture) ->
    % OTP 23 made `:get_stacktrace()` always return an empty list;
    % OTP 24 will remove it entirely.
    ?assertEqual([], WithoutTransformST),
    ?assertMatch(
       [{_Module, _Function, _ArtityOrArgs, _Info} | _],
       WithTransformST);
assert_expected_stacktraces(Function, CaseName, WithoutTransformST, WithTransformST)
  when Function =:= raise21, (CaseName =:= nested_var_capture_with_both orelse
                              CaseName =:= var_capture) ->
    assert_stacktrace_equivalence(WithoutTransformST, WithTransformST);
assert_expected_stacktraces(_Function, _CaseName, WithoutTransformST, WithTransformST) ->
    ?assertEqual([], WithoutTransformST),
    ?assertEqual([], WithTransformST).

-else.
assert_expected_stacktraces(_, _CaseName, WithoutTransformST, WithTransformST) ->
    assert_stacktrace_equivalence(WithoutTransformST, WithTransformST).

-endif. % -ifdef(POST_OTP_22)

assert_stacktrace_equivalence([{ModuleA, FunctionA, ArityOrArgsA, _InfoA} | NextA],
                              [{ModuleB, FunctionB, ArityOrArgsB, _InfoB} | NextB]) ->
    ?assertEqual(ModuleA, ModuleB),
    ?assertEqual(FunctionA, FunctionB),
    ?assertEqual(ArityOrArgsA, ArityOrArgsB),
    assert_stacktrace_equivalence(NextA, NextB);
assert_stacktrace_equivalence([], []) ->
    ok.

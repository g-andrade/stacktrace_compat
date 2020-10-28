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

-module(test_module).

% Keep `rebar3' away from warning-inducing code
% on recent versions of OTP (due to
% `erlang:get_stacktrace()' deprecation.)
%
-ifdef(COMPILING_WITHIN_TEST_SUITE).

%% ------------------------------------------------------------------
%% API function Exports
%% ------------------------------------------------------------------

-export([raise/1]).

-ifdef(POST_OTP_20).
-export([raise21/1]).
-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

raise(naked_capture) ->
    {naked_capture, erlang:get_stacktrace()};
raise(throw_capture_pattern) ->
    try
        throw(throw_capture_pattern)
    catch
        throw_capture_pattern ->
            {throw_capture_pattern, erlang:get_stacktrace()}
    end;
raise(capture_after_variable_export) ->
    try
        error(capture_after_variable_export)
    catch
        error:capture_after_variable_export ->
            {capture_after_variable_export, erlang:get_stacktrace()}
    end,

    try
        error(capture_after_variable_export)
    catch
        error:capture_after_variable_export ->
            {capture_after_variable_export, erlang:get_stacktrace()}
    end;
raise(Reason) ->
    try
        error(Reason)
    catch
        error:no_capture ->
            {no_capture, []};
        error:function_capture ->
            {function_capture, erlang:get_stacktrace()};
        error:nested_function_capture ->
            try
                error({nested, Reason})
            catch
                error:{nested, Reason} ->
                    {Reason, erlang:get_stacktrace()}
            end;
        error:nested_function_capture_with_both ->
            Stacktrace1 = erlang:get_stacktrace(),
            try
                error({nested, Reason})
            catch
                error:{nested, Reason} ->
                    Stacktrace2 = erlang:get_stacktrace(),
                    {Reason, Stacktrace1 ++ Stacktrace2}
            end;
        error:function_capture_in_expression ->
            try
                try
                    error(Reason)
                catch
                    error:Reason ->
                        {Reason, erlang:get_stacktrace()}
                end
            catch
                error:Reason ->
                    no_no
            end;
        error:function_capture_in_result_handler ->
            try 1 of
                1 ->
                    try
                        error(Reason)
                    catch
                        error:Reason ->
                            {Reason, erlang:get_stacktrace()}
                    end
            catch
                error:Reason ->
                    no_no
            end;
        error:helper_capture ->
            {helper_capture, helper()}
    end.

-ifdef(POST_OTP_20).
raise21(Reason) ->
    try
        error(Reason)
    catch
        error:unused_var_with_function_capture:Stacktrace ->
            {unused_var_with_function_capture, erlang:get_stacktrace()};
        error:var_capture:Stacktrace ->
            {var_capture, Stacktrace};
        error:nested_var_capture_with_both:Stacktrace1 ->
            try
                error(Reason)
            catch
                error:Reason:Stacktrace2 ->
                    {Reason, Stacktrace1 ++ Stacktrace2}
            end
    end.
-endif.

helper() ->
    erlang:get_stacktrace().

-endif. % COMPILING_WITHIN_TEST_SUITE

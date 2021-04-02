%% @copyright (c) 2018-2021 Guilherme Andrade
%% @private
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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(stacktrace_transform).

-deprecated(module).

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export([parse_transform/2]).

-ignore_xref([parse_transform/2]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec parse_transform(erl_syntax:forms(), []) -> erl_syntax:forms().

-ifdef(POST_OTP_20).
parse_transform(AST, _Options) ->
    %write_terms("ast_before.txt", AST),
    MappedAST = lists:map(fun map_ast_statement/1, AST),
    %write_terms("ast_after.txt", MappedAST),
    MappedAST.
-else.
parse_transform(AST, _Options) ->
    AST.
-endif.

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-ifdef(POST_OTP_20).
-record(state, {
          % amount of assigned stacktrace vars within a function (used or not)
          var_counter :: non_neg_integer(),
          % stacktrace vars within nested try catch blocks (assigned or not)
          var_stack :: [var()]
         }).

-record(var, {
          name :: atom(),
          used :: boolean()
         }).
-type var() :: #var{}.
-endif.

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

-ifdef(POST_OTP_20).
map_ast_statement({function, Line, Name, Arity, Clauses}) ->
    InitialState =
        #state{
           var_counter = 0,
           var_stack = []
          },
    MappedClauses = [element(1, walk_statements(Clause, InitialState))
                     || Clause <- Clauses],
    {function, Line, Name, Arity, MappedClauses};
map_ast_statement(Statement) ->
    Statement.

walk_statements({'try', Line,     % try
                 Expression,      % expression
                 ResultPatterns,  % of ...
                 CatchPatterns,   % catch ...
                 AfterExpressions % after ...
                },
                State) ->
    {MappedExpression, State2} = walk_statements(Expression, State),
    {MappedResultPatterns, State3} = walk_statements(ResultPatterns, State2),
    {MappedCatchPatterns, State4} =
        lists:mapfoldl(fun mapfoldl_catch_pattern/2, State3, CatchPatterns),
    {MappedAfterExpressions, State5} = walk_statements(AfterExpressions, State4),
    {{'try',
      Line,
      MappedExpression,
      MappedResultPatterns,
      MappedCatchPatterns,
      MappedAfterExpressions
     },
     State5};
walk_statements({call, Line,
                 % erlang:get_stacktrace()
                 {remote, _RemoteLine,
                  {atom, _ModuleLine, erlang},
                  {atom, _FunctionLine, get_stacktrace}},
                 [] = _Args} = Statement,
                State) ->
    case State#state.var_stack of
        [Var | StackTail] ->
            % replace call to erlang:get_stacktrace() with latest assigned stacktrace var
            MappedStatement = {var, Line, Var#var.name},
            UpdatedVar = Var#var{ used = true },
            UpdatedState = State#state{ var_stack = [UpdatedVar | StackTail] },
            {MappedStatement, UpdatedState};
        [] ->
            % no vars available for replacement
            {Statement, State}
    end;
walk_statements(Statement, State) when is_tuple(Statement) ->
    % very lazy way of walking the whole thing without explicit patterning
    % of all children formats
    StatementParts = tuple_to_list(Statement),
    {MappedStatementParts, UpdatedState} =
        walk_statements(StatementParts, State),
    MappedStatement = list_to_tuple(MappedStatementParts),
    {MappedStatement, UpdatedState};
walk_statements(Statements, State) when is_list(Statements) ->
    lists:mapfoldl(fun walk_statements/2, State, Statements);
walk_statements(StatementPart, State) ->
    {StatementPart, State}.

mapfoldl_catch_pattern({clause, Line,
                        % catch Class:Reason:Stacktrace?
                        [{tuple, TupleLine,
                          [ClassExpression,
                           ReasonExpression,
                           StExpression
                          ]}],
                         % when ...
                         Guards,
                         % ->
                         Body
                       },
                       State) ->

    {MappedStExpression, FinalMappedBody, UpdatedState} =
        case StExpression of
            {var, StExpressionLine, '_'} ->
                % Let's tentatively assign stacktrace to a var
                VarCounter = State#state.var_counter,
                VarStack = State#state.var_stack,
                VarCounter2 = VarCounter + 1,
                NewVarName = generate_st_var(VarCounter2),
                NewVar = #var{ name = NewVarName, used = false },
                VarStack2 = [NewVar | VarStack],
                State2 = State#state{ var_counter = VarCounter2, var_stack = VarStack2 },
                {MappedBody, State3} = walk_statements(Body, State2),

                case State3#state.var_stack of
                    [#var{ name = NewVarName, used = false } | VarStack] ->
                        % Stacktrace var was not used
                        {StExpression, MappedBody,
                         State3#state{ var_stack = VarStack }};
                    [#var{ name = NewVarName, used = true } | VarStack] ->
                        % Stacktrace var was used
                        {{var, StExpressionLine, NewVarName}, MappedBody,
                         State3#state{ var_stack = VarStack }}
                end;
            {var, _StExpressionLine, ExplicitStVar} ->
                % Stacktrace has been explicitly assigned
                VarStack = State#state.var_stack,
                ExplicitVar = #var{ name = ExplicitStVar, used = false },
                VarStack2 = [ExplicitVar | VarStack],
                State2 = State#state{ var_stack = VarStack2 },
                {MappedBody, State3} = walk_statements(Body, State2),
                [#var{} | VarStack] = State3#state.var_stack,
                {StExpression, MappedBody,
                 State3#state{ var_stack = VarStack }}
        end,

    %%%%%%%
    MappedClause =
        {clause, Line,
         % catch Class:Reason:Stacktrace
         [{tuple, TupleLine,
           [ClassExpression,
            ReasonExpression,
            MappedStExpression
           ]}],
         % when ...
         Guards,
         % ->
         FinalMappedBody
        },
    {MappedClause, UpdatedState}.

generate_st_var(Nr) ->
    Prefix = "StacktraceCompat444353487_",
    Suffix = integer_to_list(Nr),
    list_to_atom(Prefix ++ Suffix).

%write_terms(FilenameSuffix, List) ->
%    {attribute, _Line, module, Module} = lists:keyfind(module, 3, List),
%    Filename = atom_to_list(Module) ++ "." ++ FilenameSuffix,
%    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
%    Text = lists:map(Format, List),
%    file:write_file(Filename, Text).
-endif.

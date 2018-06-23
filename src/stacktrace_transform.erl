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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(stacktrace_transform).

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export([parse_transform/2]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

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
%% Internal Function Definitions
%%-------------------------------------------------------------------

-ifdef(POST_OTP_20).
map_ast_statement({function, Line, Name, Arity, Clauses}) ->
    InitialStVarStack = [],
    {MappedClauses, _} = walk_statements(Clauses, InitialStVarStack),
    {function, Line, Name, Arity, MappedClauses};
map_ast_statement(Statement) ->
    Statement.

walk_statements({'try', Line,     % try
                 Expression,      % expression
                 ResultPatterns,  % of ...
                 CatchPatterns,   % catch ...
                 AfterExpressions % after ...
                },
                StVarStack) ->
    {MappedExpression, StVarStack2} = walk_statements(Expression, StVarStack),
    {MappedResultPatterns, StVarStack3} = walk_statements(ResultPatterns, StVarStack2),
    {MappedCatchPatterns, StVarStack4} =
        lists:mapfoldl(fun mapfoldl_catch_pattern/2, StVarStack3, CatchPatterns),
    {MappedAfterExpressions, StVarStack5} = walk_statements(AfterExpressions, StVarStack4),
    {{'try',
      Line,
      MappedExpression,
      MappedResultPatterns,
      MappedCatchPatterns,
      MappedAfterExpressions
     },
     StVarStack5};
walk_statements({call, Line,
                 % erlang:get_stacktrace()
                 {remote, _RemoteLine,
                  {atom, _ModuleLine, erlang},
                  {atom, _FunctionLine, get_stacktrace}},
                 [] = _Args} = Statement,
                StVarStack) ->
    case StVarStack of
        [{st_var, StVar, UseCount} | StVarStackTail] ->
            % replace call to erlang:get_stacktrace() with latest assigned stacktrace var
            MappedStatement = {var, Line, StVar},
            UpdatedStVarStack = [{st_var, StVar, UseCount + 1} | StVarStackTail],
            {MappedStatement, UpdatedStVarStack};
        [] ->
            % no vars available for replacement
            {Statement, StVarStack}
    end;
walk_statements(Statement, StVarStack) when is_tuple(Statement) ->
    % very lazy way of walking the whole thing without explicit patterning
    % of all children formats
    StatementParts = tuple_to_list(Statement),
    {MappedStatementParts, UpdatedStVarStack} =
        walk_statements(StatementParts, StVarStack),
    MappedStatement = list_to_tuple(MappedStatementParts),
    {MappedStatement, UpdatedStVarStack};
walk_statements(Statements, StVarStack) when is_list(Statements) ->
    lists:mapfoldl(fun walk_statements/2, StVarStack, Statements);
walk_statements(StatementPart, StVarStack) ->
    {StatementPart, StVarStack}.

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
                       StVarStack) ->

    {MappedStExpression, FinalMappedBody} =
        case StExpression of
            {var, StExpressionLine, '_'} ->
                % Let's tentatively assign stacktrace to a var
                ImplicitStVarNr = length(StVarStack) + 1,
                ImplicitStVar = generate_st_var(ImplicitStVarNr),
                StVarStack2 = [{st_var, ImplicitStVar, 0} | StVarStack],
                {MappedBody, StVarStack3} = walk_statements(Body, StVarStack2),
                case hd(StVarStack3) of
                    {st_var, ImplicitStVar, 0} ->
                        % Stacktrace var was not used; discard it
                        {StExpression, MappedBody};
                    {st_var, ImplicitStVar, _Count} ->
                        {{var, StExpressionLine, ImplicitStVar},
                         MappedBody}
                end;
            {var, _StExpressionLine, ExplicitStVar} ->
                % Stacktrace has been explicitly assigned
                UpdatedStVarStack = [{st_var, ExplicitStVar, 0} | StVarStack],
                {MappedBody, _} = walk_statements(Body, UpdatedStVarStack),
                {StExpression, MappedBody}
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
    {MappedClause, StVarStack}.

generate_st_var(Nr) ->
    Prefix = "StacktraceCompat444353487_",
    Suffix = integer_to_list(Nr),
    list_to_atom(Prefix ++ Suffix).

%write_terms(Filename, List) ->
%    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
%    Text = lists:map(Format, List),
%    file:write_file(Filename, Text).
-endif.

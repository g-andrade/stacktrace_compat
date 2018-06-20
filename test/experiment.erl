-module(experiment).

-compile({parse_transform, stacktrace_transform}).

%% API exports
-export([do_it/2]).

%%====================================================================
%% API functions
%%====================================================================

do_it(Class, Reason) ->
    try
        erlang:Class(Reason)
    catch
        error:nothing ->
            meh;
        error:separate ->
            {separate, erlang:get_stacktrace()};
        error:separate_too:_ ->
            {separate_too, erlang:get_stacktrace()};
        error:redundant:Stacktrace ->
            {redundant, erlang:get_stacktrace()};
        error:fine:Stacktrace ->
            {fine, Stacktrace};
        error:conflict ->
            try
                erlang:raise(Class, Reason, erlang:get_stacktrace())
            catch
                error:conflict ->
                    {conflict, erlang:get_stacktrace()}
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

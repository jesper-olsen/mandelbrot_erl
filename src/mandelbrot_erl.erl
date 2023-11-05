-module(mandelbrot_erl).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    mandelbrot:plot_ascii({-1.20, 0.20}, {-1.0, 0.35}, {60, 30}),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

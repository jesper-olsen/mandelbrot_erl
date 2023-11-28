-module(mandelbrot_erl).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    %io:format("Args: ~p~n", [Args]),
    argparse:run(Args, cli(), #{progname => mandelbrot_erl}),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
cli() ->
    #{arguments =>
          [#{name => height,
             short => $h,
             type => integer,
             default => 750},
           #{name => width,
             short => $w,
             type => integer,
             default => 1000},
           #{name => parallel,
             short => $p,
             type => boolean,
             default => false}],
      handler =>
          fun(#{height := Height,
                width := Width,
                parallel := P}) ->
             LL = {-1.20, 0.20},
             UR = {-1.00, 0.35},
             mandelbrot:plot(P, ascii, LL, UR, {60, 30}),
             mandelbrot:plot(P, png, LL, UR, {Width, Height})
          end}.

-module(mandelbrot).

-include_lib("eunit/include/eunit.hrl").

-export([plot/5]).

render_ascii([]) ->
    ok;
render_ascii([ROW | REST_ROWS]) ->
    LINE=lists:foldl(fun(Z, S) -> [cnt2char(Z) | S] end, [], ROW),
    io:format("~s~n", [lists:reverse(LINE)]),
    render_ascii(REST_ROWS).

symbols() -> "MW2a_. ".
cnt2char(N) when N>=0 ->
    Idx = round((N/ 255) * (length(symbols()) - 1)),
    [lists:nth(Idx + 1, symbols())].

example() ->
    plot(false, ascii, {-1.20,0.20}, {-1.0,0.35},{60,30}).

plot(Parallel, Type, LowerLeft, UpperRight, Bound) ->
    Pixels =
        case Parallel of
            false ->
                calc_pixels(LowerLeft, UpperRight, Bound);
            true ->
                pcalc_pixels(LowerLeft, UpperRight, Bound)
        end,
    case Type of
        ascii ->
            render_ascii(Pixels);
        png ->
            png:make_gray_png("mandelbrot.png", Pixels)
    end.

escape(_, _, Limit, N) when N >= Limit ->
    N;
escape({Zr, Zi}, {Cr, Ci}, Limit, N) ->
    % Note faster to calculate Zn here than in true clause, because
    % the beam will otherwise not keep it in a special register...
    Zn = {Zr * Zr - Zi * Zi + Cr, 2 * Zi * Zr + Ci}, % Z*Z + C
    case Zr * Zr + Zi * Zi < 2.0 of
        false ->
            N;
        true ->
            escape(Zn, {Cr, Ci}, Limit, N + 1)
    end.

calc_pixels({LLx, LLy}, {URx, URy}, {WIDTH, HEIGHT}) ->
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    I = [URy - Y * (URy - LLy) / HEIGHT || Y <- lists:seq(0, HEIGHT -1)],
    [[255 - escape({0.0, 0.0}, {X, Y}, 255, 0) || X <- R] || Y <- I].

pcalc_pixels({LLx, LLy}, {URx, URy}, {WIDTH, HEIGHT}) ->
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    I = [URy - Y * (URy - LLy) / HEIGHT || Y <- lists:seq(0, HEIGHT -1)],
    ROWS = [[{X, Y} || X <- R] || Y <- I],
    pmap(fun(Row) -> [255 - escape({0.0, 0.0}, C, 255, 0) || C <- Row] end, ROWS).

pmap(F, L) ->
    S = self(),

    Ref = erlang:make_ref(), % unique
    Pids = lists:map(fun(I) -> spawn(fun() -> do_f(S, Ref, F, I) end) end, L),
    %% gather the results
    gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, catch F(I)}.

gather([Pid | T], Ref) ->
    receive
        {Pid, Ref, Ret} ->
            [Ret | gather(T, Ref)]
    end;
gather([], _) ->
    [].

escape_test1() ->
    ?assert(escape({0.0, 0.0}, {0.0, 0.0}, 255, 0) =:= 255).

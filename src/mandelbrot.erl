-module(mandelbrot).

-include_lib("eunit/include/eunit.hrl").

-export([plot/5]).

%-compile(export_all).

render_ascii([]) ->
    ok;
render_ascii([Row | RestRows]) ->
    render_ascii_row(Row),
    render_ascii(RestRows).

render_ascii_row([]) ->
    io:format("~n");
render_ascii_row([H | T]) ->
    io:format(cnt2char(H)),
    render_ascii_row(T).

cnt2char(N) when N >= 200 ->
    " ";
cnt2char(N) when N >= 150 ->
    ".";
cnt2char(N) when N >= 100 ->
    "_";
cnt2char(N) when N >= 70 ->
    "a";
cnt2char(N) when N >= 30 ->
    "2";
cnt2char(N) when N >= 15 ->
    "W";
cnt2char(N) when N >= 0 ->
    "M".

% mandelbrot:plot(sequential, ascii, {-1.20,0.20}, {-1.0,0.35},{60,30}).
plot(Parallel, Type, LowerLeft, UpperRight, Bound) ->
    Pixels = case Parallel of
            false  ->
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

escape(_, _, Limit, It) when It >= Limit ->
    It;
escape({Zr, Zi}, {Cr, Ci}, Limit, It) ->
    case Zr * Zr + Zi * Zi < 2.0 of
        false ->
            It;
        true ->
            Zn = {Zr * Zr - Zi * Zi + Cr, 2 * Zi * Zr + Ci}, % Z*Z + C
            escape(Zn, {Cr, Ci}, Limit, It + 1)
    end.

% mandelbrot:calc_pixels({-1.20,0.20}, {-1.0,0.35},{60,30}).
calc_pixels({LLx, LLy}, {URx, URy}, {WIDTH, HEIGHT}) ->
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    C = [LLy + Y * (URy - LLy) / HEIGHT || Y <- lists:seq(HEIGHT - 1, 0, -1)],
    [[255 - escape({0.0, 0.0}, {X, Y}, 255, 0) || X <- R] || Y <- C].

pcalc_pixels({LLx, LLy}, {URx, URy}, {WIDTH, HEIGHT}) ->
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    C = [LLy + Y * (URy - LLy) / HEIGHT || Y <- lists:seq(HEIGHT - 1, 0, -1)],
    ROWS = [[{X, Y} || X <- R] || Y <- C],
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

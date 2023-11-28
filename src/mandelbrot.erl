-module(mandelbrot).

-include_lib("eunit/include/eunit.hrl").

-export([plot/5]).
%-compile(export_all).

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
                pcalc_pixels2(LowerLeft, UpperRight, Bound)
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

pcalc_pixels(LL, UR, BOUND) ->
    ROWS = calc_rows(LL, UR, BOUND),
    pmap(fun(Row) -> escape_row(Row) end, ROWS).

escape_row(Row) ->
    [255 - escape({0.0, 0.0}, C, 255, 0) || C <- Row].

calc_rows({LLx, LLy}, {URx, URy}, {WIDTH, HEIGHT}) ->
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    I = [URy - Y * (URy - LLy) / HEIGHT || Y <- lists:seq(0, HEIGHT -1)],
    [[{X, Y} || X <- R] || Y <- I].

filter_results([]) -> [];
filter_results([{Y,R} |T]) -> [R | filter_results(T)].

pcalc() -> pcalc_pixels2({-1.20,0.20}, {-1.0,0.35},{60,30}).

% split and spawn - n workers
pcalc_pixels2(LL, UR, BOUND) ->
    S = self(),
    F = fun(Row) -> escape_row(Row) end,
    Workers = [spawn_link(fun() -> worker(S, F) end) || _ <- lists:seq(0,20)],
    ROWS = calc_rows(LL, UR, BOUND),
    sup_workers(Workers, ROWS, length(ROWS), []).

sup_workers([Worker | Workers], [Job | Jobs], N, Results) ->
    Worker ! {self(), {length(Jobs),Job}},
    sup_workers(Workers, Jobs, N, Results);

sup_workers([], [], 0, Results) -> 
    R=lists:sort(fun({A,_},{B,_}) -> A>B end, Results),
    filter_results(R);

sup_workers([], Jobs, N, Results) ->
    receive
        {Pid, {Y,R}} ->
            sup_workers([Pid | []], Jobs, N-1, [{Y,R} | Results]) 
    end;
sup_workers([Worker | Workers], [], N, Results) ->
    Worker ! self(),
    sup_workers(Workers, [], N, Results).

worker(Parent, F) ->
    receive
        {Parent, {Y,Row}} -> 
            Parent ! {self(), {Y,F(Row)}},
            worker(Parent, F);
        {Parent} -> 
            exit(normal) 
    end. 

% pmap - works like map, except elements processed in parallel
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

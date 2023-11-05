-module(mandelbrot).

-export([plot_ascii/3]).

%-compile(export_all).

render_ascii([], _, _) ->
    io:format("~n");
render_ascii([H | T], I, WIDTH) when I rem WIDTH =:= 0 ->
    io:format("~n" ++ cnt2char(H)),
    render_ascii(T, I + 1, WIDTH);
render_ascii([H | T], I, WIDTH) ->
    io:format(cnt2char(H)),
    render_ascii(T, I + 1, WIDTH).

cnt2char(N) when N >= 500 ->
    "M";
cnt2char(N) when N >= 400 ->
    "W";
cnt2char(N) when N >= 300 ->
    "2";
cnt2char(N) when N >= 200 ->
    "a";
cnt2char(N) when N >= 100 ->
    "_";
cnt2char(N) when N >= 50 ->
    ".";
cnt2char(N) when N >= 0 ->
    " ".

% mandelbrot:plot_ascii({-1.20,0.20}, {-1.0,0.35},{60,30}).
plot_ascii(LowerLeft, UpperRight, Bound) ->
    {LLx, LLy} = LowerLeft,
    {URx, URy} = UpperRight,
    {WIDTH, HEIGHT} = Bound,
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    C = [LLy + Y * (URy - LLy) / HEIGHT || Y <- lists:seq(HEIGHT - 1, 0, -1)],
    L = [{X, Y} || Y <- C, X <- R],
    M = [escape(W, 500, 0) || W <- L],
    render_ascii(M, 0, WIDTH).

escape(C, Limit, It) ->
    escape({0.0, 0.0}, C, Limit, It).

escape(_, _, Limit, It) when It >= Limit ->
    It;
escape(Z, C, Limit, It) ->
    case complex:norm_sqr(Z) < 2.0 of
        false ->
            It;
        true ->
            escape(complex:add(
                       complex:mul(Z, Z), C),
                   C,
                   Limit,
                   It + 1)
    end.

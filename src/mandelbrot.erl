-module(mandelbrot).

-export([plot/4]).

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

% mandelbrot:plot_ascii({-1.20,0.20}, {-1.0,0.35},{60,30}).
plot(Type, LowerLeft, UpperRight, Bound) ->
    P = calc_pixels(LowerLeft, UpperRight, Bound),
    case Type of
        ascii ->
            render_ascii(P);
        png ->
            png:make_gray_png("mandelbrot.png", P)
    end.

calc_pixels(LowerLeft, UpperRight, Bound) ->
    {LLx, LLy} = LowerLeft,
    {URx, URy} = UpperRight,
    {WIDTH, HEIGHT} = Bound,
    R = [LLx + X * (URx - LLx) / WIDTH || X <- lists:seq(0, WIDTH - 1)],
    C = [LLy + Y * (URy - LLy) / HEIGHT || Y <- lists:seq(HEIGHT - 1, 0, -1)],
    [[255 - escape({X, Y}, 255, 0) || X <- R] || Y <- C].

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

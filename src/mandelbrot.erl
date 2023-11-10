-module(mandelbrot).

-define(ADD(Rr, Ir, R1, I1, R2, I2),
        begin
            Rr = R1 + R2,
            Ir = I1 + I2
        end).
-define(NORMSQ(R, I), R * R + I * I).
-define(SQUARE(Rr, Ir, R, I),
        begin
            Rr = R * R - I * I,
            Ir = 2 * R * I
        end).

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
    [[255 - escape(0.0, 0.0, X, Y, 255, 0) || X <- R] || Y <- C].

escape(R, I, Rc, Ic, Limit, It) ->
    if It >= Limit ->
           It;
       ?NORMSQ(R, I) >= 2.0 ->
           It;
       true ->
           ?SQUARE(Rs, Is, R, I),
           ?ADD(Rx, Ix, Rs, Is, Rc, Ic),
           escape(Rx, Ix, Rc, Ic, Limit, It + 1)
    end.

-module(png).

-export([make_gray_png/1, make_gray_png/2, example/0]).

%-compile(export_all).

make_gray_png(ROWS) ->
    {Height, Width} = {length(ROWS), lists:max([length(Row) || Row <- ROWS])},
    Png = <<16#89, "PNG\r\n\x1A\n">>,
    IHDR = make_ihdr(Width, Height),
    IDAT = make_idat(list_to_idat_raw(ROWS)),
    IEND = <<0:32, "IEND">>,
    CRC = erlang:crc32(IEND),
    <<Png/binary, IHDR/binary, IDAT/binary, IEND/binary, CRC:32>>.

make_ihdr(Width, Height) ->
    Colourtype = 0, % gray - no palette
    Bitdepth = 8, % 0..255
    Compression = 0, % zlib
    Filtertype = 0, % adaptive, each scanline separately
    Interlaced = 0, % no
    IHDR =
        <<Width:32,
          Height:32,
          Bitdepth:8,
          Colourtype:8,
          Compression:8,
          Filtertype:8,
          Interlaced:8>>,
    Chunk = <<"IHDR", IHDR/binary>>,
    CRC = erlang:crc32(Chunk),
    BSIZE = byte_size(IHDR),
    <<BSIZE:32, Chunk/binary, CRC:32>>.

list_to_idat_raw(ROWS) ->
    L = lists:map(fun(L) -> list_to_binary(L) end, ROWS),
    % each row starts with 0 => no filter for scanline
    Raw = lists:foldl(fun(R, D) -> <<D/binary, 0:8, R/binary>> end, <<>>, L).

make_idat(Raw) ->
    Z = zlib:open(),
    zlib:deflateInit(Z),
    Compressed = list_to_binary(zlib:deflate(Z, Raw, finish)),
    zlib:deflateEnd(Z),
    BSIZE = byte_size(Compressed),
    Chunk = <<"IDAT", Compressed/binary>>,
    CRC = erlang:crc32(Chunk),
    <<BSIZE:32, Chunk/binary, CRC:32>>.

make_gray_png(Fname, Data) ->
    erlang:display("Saving to " ++ Fname),
    PngData = make_gray_png(Data),
    {ok, File} = file:open(Fname, [write, binary]),
    file:write(File, PngData),
    file:close(File).

example() ->
    Data = [[0, 255, 0], [255, 255, 255], [0, 255, 0]],
    make_gray_png("cross3x3.png", Data).

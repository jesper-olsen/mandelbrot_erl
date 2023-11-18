mandelbrot_erl
==============

An escript for calculating the mandelbrot set.

Can run sequentially or spawn multiple processes.

Build
-----

    $ rebar3 escriptize

Usage
-----
```
Usage:
  mandelbrot_erl [-p] [-h <height>] [-w <width>]

Optional arguments:
  -h height (int, 750)
  -w width (int, 1000)
  -p parallel (false)
```


Run
---
    $ _build/default/bin/mandelbrot_erl

which will produce an ascii result                                                             
```
  .                                                       . 
    M                                                       
             .                                              
         .                                                  
                         .                                 .
            .           M                                  a
             _         .                            .. ...MM
          .  _M2..                                     .MMMM
         _ .aMMMM _ . ..a               .  .          aaMMMM
             MMMaMMMMM.Ma   .             _.        .  .MMMM
             WMMMMMMMMMM_M .           ._ ..           .MMMM
     .     _2MMMMMMMMMMMMMM.              _M2M  ._. M   .MMM
           MMMMMMMMMMMMMMM2         ..  aMMMMM__MMMMMMMMaMMM
       a aMMMMMMMMMMMMMMMMM_       _M. M _MMMMMMMMMMMMMMMMMM
   .     _MMMMMMMMMMMMMMMMMM    2.a.MMMMMMMMMMMMMMMMMMMMMMMM
          MMMMMMMMMMMMMMMMM.  2_ MMMMMMMMMMMMMMMMMMMMMMMMMMM
          .2MMMMMMMMMMMMMMM  MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
          _MMMMMMMMMMMMMMM.MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
           ._MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
           . ..MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
                  .MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
                .MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
            ._MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
```

as well as a PNG result
![PNG](https://github.com/jesper-olsen/mandelbrot_erl/main/mandelbrot.png) 

Benchmark
---------

By default the area with lower left {-1.20,0.20} and upper right {-1.0,0.35} is mapped.

In parallel mode (option -p) the program spawns a separate process per image row. In the benchmark we keep the number of pixels fixed at 25 million and vary the number of processes spawned.

All the times below are when running on a Macbook Air M1 (2020).

```
$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p
```

| Width  | Height | Parallel | Time (sec) |
| ------:| ------:| :------: | ---------: |
| 5000   | 5000   | false    | 61         |
| 5000   | 5000   | true     | 27         |
| 50000  | 500    | false    | 61         |
| 50000  | 500    | true     | 17         |
| 500000 | 50     | false    | 72         |
| 500000 | 50     | true     | 19         |
| 5000000| 5      | false    | 135        |
| 5000000| 5      | true     | 27         |

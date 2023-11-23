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
                                                .           
                                                .           
                                               .            
                                                .           
                                               ..        .  
              .                                  ...  .. .  
 .             .                                       ...  
..             ..                                       ... 
 ..           ..                                        ....
 ...M. .     ...                                       .....
   .......  ...         .                               ....
       .._....          .                                 ..
  .     ......  ..      ._                             ...._
           ........  ...M                            ......2
          ...a...... .._....                     ...._.___MM
         ....aMW__.........          ..            ...._MMMM
        .a..2MMMM.a._.__2.. .         .._.._        ..22MMMM
        .....MMM2MMMMM_M2..._          ...a_..     ._.._MMMM
         ....WMMMMMMMMMMaM._..        ._a._............_MMMM
    ._.....aWMMMMMMMMMMMMMM_..   ..  .....aMWM.._a_.M..._MMM
     ......MMMMMMMMMMMMMMMW... .....__..2MMMMMaaMMMMMMMM2MMM
  .....2.2MMMMMMMMMMMMMMMMMa.......aM_.M.aMMMMMMMMMMMMMMMMMM
 ........aMMMMMMMMMMMMMMMMMM....W_2.MMMMMMMMMMMMMMMMMMMMMMMM
       ...MMMMMMMMMMMMMMMMM_..Wa.MMMMMMMMMMMMMMMMMMMMMMMMMMM
       ..._WMMMMMMMMMMMMMMM..MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
       ...aMMMMMMMMMMMMMMM_MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
      ....._aMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
          ._.__MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
          .........MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
         ......._MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
```

as well as a PNG result
![PNG](https://raw.githubusercontent.com/jesper-olsen/mandelbrot_erl/main/mandelbrot.png) 

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

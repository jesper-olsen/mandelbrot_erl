mandelbrot_erl
==============

An escript for calculating the mandelbrot set.  Can run sequentially or spawn multiple processes.

### Other Language Implementations

This project compares the performance and features of Mandelbrot set generation in different languages.
Single Thread/Multi-thread shows the number of seconds it takes to do a 5000x5000 calculation.


| Language    | Repository                                                           | Single Thread   | Multi-Thread | Simd | Multi-Thread + Simd |
| :--------   | :------------------------------------------------------------------- | ---------------:| -----------: | ----:| ------------------: |
| Awk         | [mandelbrot-awk](https://github.com/jesper-olsen/mandelbrot-awk)     |           417.9 |              |      |                     |
| C           | [mandelbrot-c](https://github.com/jesper-olsen/mandelbrot-c)         |             3.6 |          0.6 |  0.7 |               0.2   |
| **Erlang**  | [mandelbrot_erl](https://github.com/jesper-olsen/mandelbrot_erl)     |            35.6 |          8.3 |      |                     |
| Fortran     | [mandelbrot-f](https://github.com/jesper-olsen/mandelbrot-f)         |             4.5 |              |      |                     |
| Go          | [mandelbrot-go](https://github.com/jesper-olsen/mandelbrot-go)       |             4.1 |          0.8 |  1.3 |               0.4   |
| Lua         | [mandelbrot-lua](https://github.com/jesper-olsen/mandelbrot-lua)     |            33.2 |              |      |                     |
| Mojo        | [mandelbrot-mojo](https://github.com/jesper-olsen/mandelbrot-mojo)   |             3.8 |          1.2 |  0.7 |               0.4   |
| Nushell     | [mandelbrot-nu](https://github.com/jesper-olsen/mandelbrot-nu)       |         17186.6 |              |      |                     |
| Odin        | [mandelbrot-odin](https://github.com/jesper-olsen/mandelbrot-odin)   |             4.4 |              |      |                     |
| Python      | [mandelbrot-py](https://github.com/jesper-olsen/mandelbrot-py)       |     (pure) 93.3 | (jax)    5.9 |      |                     |
| R           | [mandelbrot-R](https://github.com/jesper-olsen/mandelbrot-R)         |           335.0 |              |      |                     |
| Rust        | [mandelbrot-rs](https://github.com/jesper-olsen/mandelbrot-rs)       |             4.7 |          1.3 |      |                     |
| Swift       | [mandelbrot-swift](https://github.com/jesper-olsen/mandelbrot-swift) |             4.5 |          1.2 |  1.3 |               0.7   |
| Tcl         | [mandelbrot-tcl](https://github.com/jesper-olsen/mandelbrot-tcl)     |           306.9 |              |      |                     |
| Zig         | [mandelbrot-zig](https://github.com/jesper-olsen/mandelbrot-zig)     |             4.9 |          0.9 |  0.7 |               0.3   |

## Prerequisites

You will need the following installed:

1. **Erlang/OTP** (this project was last verified against OTP 29). (On macOS: `brew install erlang`).
2. **rebar3**, the build tool used below (brew install rebar3).
3. **Gnuplot** (required *only* for generating PNG images).

Verify:
```sh
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' 
rebar3 --version
```
```sh
29
rebar 3.27.0 on Erlang/OTP 29 Erts 17.0.5
```

### Build
-----

    $ rebar3 escriptize

### Usage
-----
```
Usage:
  mandelbrot_erl [-p] [-h <height>] [-w <width>]

Optional arguments:
  -h height (int, 750)
  -w width (int, 1000)
  -p parallel (int, 1)
```


### Run
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

### Benchmark
---------

Below we will benchmark the time it takes to calculate a 25M pixel mandelbrot on a Macbook Air M5. All times are in seconds, and by the defaults it is the area with lower left {-1.20,0.20} and upper right {-1.0,0.35} that is mapped.

Spawn exactly #Workers processes - supervisor sends them one row at a 
time to process and collects/merges the results as they come in.

```
$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 1
32.75s user 1.54s system 96% cpu 35.644 total

$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 2
33.15s user 1.64s system 176% cpu 19.665 total

$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 4
34.87s user 1.43s system 300% cpu 12.073 total

$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 8
40.25s user 1.70s system 482% cpu 8.692 total

$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 16
43.74s user 1.93s system 552% cpu 8.263 total

$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 32
43.50s user 2.06s system 534% cpu 8.521 total

$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 5000
 89.61s user 2.49s system 701% cpu 13.125 total
```

| #Workers | Time (real) |  Speedup |
| -------: | ---------:  |  ------: |
|  1       | 35.6        | 
|  2       | 19.7        |  1.6     |
|  4       |  8.7        |  3.5     |
|  8       |  8.3        |  3.7     |
| 16       |  8.5        |  3.6     |
| 32       |  8.5        |  3.6     |
| 5000     | 13.1        |  2.3     |


### References
-------
[1] Programming Erlang, 2nd Ed, Joe Armstrong

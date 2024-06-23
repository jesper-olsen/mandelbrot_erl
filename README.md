mandelbrot_erl
==============

An escript for calculating the mandelbrot set.  Can run sequentially or spawn multiple processes.
Other languages:
* [Rust](https://github.com/jesper-olsen/mandelbrot-rs) 
* [Fortran](https://github.com/jesper-olsen/mandelbrot-f) 
* [Python](https://github.com/jesper-olsen/mandelbrot-py) 
* [Mojo](https://github.com/jesper-olsen/mandelbrot-mojo) 


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
  -p parallel (int, 1)
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

Below we will benchmark the time it takes to calculate a 25M pixel mandelbrot on a Macbook Air M1 (2020, 8 cores). All times are in seconds, and by the defaults it is the area with lower left {-1.20,0.20} and upper right {-1.0,0.35} that is mapped.

Three different modes are explored below: sequential, pmap and "split and spawn". The latter two spawn multiple processes and should run faster on multi core CPUs.


```
$ time _build/default/bin/mandelbrot_erl -w 5000 -h 5000 -p 1

```
### Sequential (calc_pixels)

| Time (real) | Time (user) | Speedup |
| ---------:  | ----------: | ------: |
| 56          | 41          |         |

### Pmap (calc_pixels_pmap)

Parallel map a la Armstrong [1]: Spawn a process per row in the image - trivial merge of individual process results.

| #Workers | Time (real) | Time (user) | Speedup |
| -------: | ---------:  | ----------: | ------: |
|  5000    | 25          | 126         |         |

### Split and Spawn (calc_pixels_split_and_spawn)

Spawn exactly #Workers processes - supervisor sends them one row at a 
time to process and collects/merges the results as they come in.

| #Workers | Time (real) | Time (user) | Speedup |
| -------: | ---------:  | ----------: | ------: |
|  1       | 53          | 49          |
|  2       | 33          | 51          | 1.6     |
|  4       | 19          | 52          | 2.8     |
|  8       | 16          | 65          | 3.3     |
| 16       | 16          | 67          | 3.3     |
| 32       | 16          | 66          | 3.3     |
| 5000     | 25          | 127         | 2.1     |


References
-------
[1] Programming Erlang, 2nd Ed, Joe Armstrong

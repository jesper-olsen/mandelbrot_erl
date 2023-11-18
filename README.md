mandelbrot_erl
==============

An escript

Build
-----

    $ rebar3 escriptize

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
![PNG](https://raw.githubusercontent.com/jesper-olsen/mandelbrot_erl/main/mandelbrot.png) 

Benchmark
---------

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

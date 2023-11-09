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

 
0.1.0.1
    pushed on hackage 2022-07-25
0.1.0.2 
    trial for a split version - no examples 
    probably not worthwile, 
    `toysolver` remains necessary, which forces base <4.15
    which does not allow to go to ghc 9.
0.1.0.3     maintenance 
-    issue with not finding include 
    trying <> instead of ""
    change both include-dir and C-dir to ./Cdir
    there was a hint on stackoverflow that single char dirs do not work well in cabal
    works with stack once, after stack install uniform-geometry can use it.
-     cannot go to hpack, trying stack init - not working either

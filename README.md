# HaskellGameOfLife
Conway's Game of Life Implemented in Haskell

The only library used is Gloss, which handles the graphics.

I'm pretty new to Haskell so don't think this codebase represents the optimal or even a good way to write Haskell. I'm not even totally sure what a monad is yet.

HOW TO SET UP AND RUN:
    You'll need GHC and Cabal installed. Gloss also requires some kind of OpenGl library installed on your machine, as well as GLUT. I've got mesa 3d and freeglut installed on my rig. I'm using arch linux. This might not work on windows. The solution to that is to stop using windows.
    Once you've got the requirements installed, clone this repo and run 'cabal run' in the GameOfLife directory.

CHANGING STARTING CONFIGURATIONS:
    You'll find a few starting configurations in InitialConfigurations.hs. By default the "Gliders by the dozen" configuration is being used, which is the most interesting looking of the coded patterns. You can change the value of initialConfiguration in Main.hs to one of these to get a different starting configuration, or you can add your own following the commented instructions in InitialConfigurations.hs. You'll have to recompile after that, just run 'cabal run' as normal after making the changes. In the future I'll add IO to allow users to select from the fixed configurations, but for now you'll have to recompile.
    I plan to add a randomized configuration in the future, but right now that feature is not supported.

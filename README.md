# HaskellGameOfLife
Conway's Game of Life implemented in Haskell

The code found in the FromHuttonBook directory is taken from the examples in chapter 10 of the second edition of Graham Hutton's Programming in Haskell.
To run it, run GHCI and then load the code by entering :l life.hs, and finally call the life function.
The program is ran by invoking the life function, which takes as its argument [(Int,Int)], a list of tuples representing the coordinates of living cells at the start of the "game".
The book provides the glider starting configuration: [(4,2),(2,3),(4,3),(3,4),(4,4)]

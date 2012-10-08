queens
======

A Clojure implementation of a variant of the Queens on a Chessboard game: given N, place N queens on a N x N squares grid such that:
 - at most one queen occupies any diagonal, row or column
 - at most two queens can occupy any other line.

Given N as input, the program calculates one, or optionally up to a number of or all, solutions.

Since the time cost is high, concurrency is used to attack and pre-compute candidate subsets of a correct solution.

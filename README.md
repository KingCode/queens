queens
======

A Clojure implementation of a variant of the Queens on a Chessboard game: given N, place N queens on a N x N squares grid such that:
 - at most one queen occupies any diagonal, row or column
 - at most two queens can occupy any other line.

Given N as input, the program calculates one, or optionally up to a number of or all, solutions.

For scalability, dynamic programming and (optionally) concurrency are used.

STATUS

This project is dormant, and needs to be redone. There is not much to do currently, except to run tests and play the game
at the repl - see console.clj

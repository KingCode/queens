queens
======

A Clojure implementation of a variant of the Queens on a Chessboard game: given N, place N queens on a N x N squares grid such that:
 - at most one queen occupies any diagonal, row or column
 - at most two queens can occupy any other line.

Given N as input, the program calculates one, or optionally up to a number of or all, solutions.

For scalability, dynamic programming and (optionally) concurrency are used.

This is an exercise to learn Clojure/leiningen and was prompted by coding challenge (https://www.interviewstreet.com/challenges/dashboard/#problem/502cb2fb4649c).

STATUS

Algorithm and data structures fleshed out for single threaded version.
 
TODOs: - implement sequential algorithm
       - design and implement concurrent approach.
       - implement web app and improve console I/O to 'play' the puzzle. 


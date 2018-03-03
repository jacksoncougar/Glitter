# NAME

glitter - polyomino board solver

# SYNOPSIS

glitter [-nc] < file...

# DESCRIPTION

Glitter places polyominos onto a board of locations and attempts to find a board solution with all polyominos placed. No polyominos can be placed overlapping with another, and no polyominos can extend outside of the board. 

A solution is still valid if there are unfilled places on the board.

## Example

This is an example program output from a 3x20 pentomino problem.

	Found a solution!
	Looks like this: 
	e e h a a a g b b b b j d l k k k f f f 
	e h h h a a g g g b j j d l l l k k i f 
	e e h c c c c c g j j d d d l i i i i f 
	Goodbye.

The file can be entered manually using the following syntax:

	[HEIGHT] [WIDTH] [ENTER] /* width and height of board*/
	[NUMBER OF POLYOMINOS]  [ENTER] /* total number of polyominals to follow */
	[NUMBER OF COORDINATES] [COORD 1] [COORD 2] ... [COORD N] [ENTER] /* first polyomino */
	...
	[NUMBER OF COORDINATES] [COORD 1] [COORD 2] ... [COORD N] [ENTER] /* last polyomino */
After pressing the last enter the program should execute immediatly.

Alternatively the problem can be passed inside a file using file redirection:

	glitter < file

# OPTIONS

	-nc     : disable ANSI colour output
	-nl     : disable polyomino labels

# BUILD

	ghc Main

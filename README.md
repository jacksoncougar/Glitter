# NAME

glitter - polyomino board solver

# SYNOPSIS

glitter [-nc] < file...

# DESCRIPTION

Glitter places polyominos onto a board of locations and attempts to find a board solution with all polyominos placed. No polyominos can be placed overlapping with another, and no polyominos can extend outside of the board. 

A solution is still valid if there are unfilled places on the board.

The file can be entered manually using the following syntax:

	[HEIGHT] [WIDTH] [ENTER]
	[NUMBER OF POLYOMINOS]  [ENTER]
	[NUMBER OF COORDINATES] [COORD 1] [COORD 2] ... [COORD N] [ENTER]

After pressing the last enter the program should execute immediatly.

Alternatively the problem can be passed inside a file using file redirection:

	glitter < file

# OPTIONS

 -nc
: disable ANSI colour output

# First attempty at day 21

The problem specification is really close to being a valid Haskell
program.  For the first half, all you have to do is change the ":"
to an "=" on each line, and it's valid Haskell code.  Adding a
one-liner to call "root" and print the answer is all it takes.

For the second half, some edits add an argument to each function,
and change the meeting of "root" to be a test whether a number
is the answer.  That was good enough for the sample problem.  
Searching 0, 1, -1, 2, -2, ... found the answer right away.
For the real problem, though, it didn't find the answer even
after running for 15 minutes.

# Connect4

A prolog implementation of a variation on the MinMax algorithm for a perfect Connect4 AI along with a Java front-end. As of right now, the algorithm has no bugs in correctness, but has efficiency issues when scaled up too high

## How to run

For all Operating Systems: Install SWI-Prolog and run the jar file. Note that Connect4Logic.pl must be in your current directory or else SWI won't be able to run the AI.

For linux systems, if you can install SWI from a repo and can call swipl,
'''
java -jar Connect3.jar
'''
With no arguments should work

For everyone else, simply add on the path to your swipl executable file. eg
'''
java -jar Connect3.jar "C:\Program Files\swipl\bin\swipl.exe"
'''
for Windows

##Current Issues

As of right now, not enough optimizations have been implemented to guarantee termination for most computers running default connect4, so the code is set to run a simplified version only requiring 3-in-a-row.

Also, as of right now, if an AI determines that it cannot force a win or tie. It just picks a random move.


%%% Name:		Timothy Wilson
%%% Class:		CSC 345
%%% Assignment:		Project 1
%%% Description:	This file contains the technical information for the environment, files, and how to run.
%%%-------------------------------------------------------------------------------------------

==> Environment

symbolic-integration was created and tested in Ubuntu 18.04 On the 4.15 kernel.
Development was through Emacs GUI + the SLIME REPL.

%%%-------------------------------------------------------------------------------------------

==> Files 

The files in this project are:
	* symbolic-integration.lisp
	* symbolic-integration-test-cases.lisp
	* output.txt
	* bugReport.txt
	* README.md
The provided files are found in the SymbolicIntegration folder in the current project structure.


====> symbolic-integration.lisp 

This file contains the core pieces to creating a proper integral calculator.
The pieces are as follows: Symbols, Operators, Operands, Predicates, Constructors, and Core.

====> symbolic-integration-test-cases.lisp

This file contains the 20 core tests that Dr. Wyatt assined, and made testing
far more simple while being perfectly robust.

====> output.txt

output.txt contains the output of successful [unsuccessful] runs of the integration calculator.

====> bugReport.txt

This file contains all the necessary information to regard upon testing the functionality
created, and the data will help assess the current state of the code.

====> README.md

This file contains all the technical information for the environment, files, and how to run.

%%%-------------------------------------------------------------------------------------------

==> How To Run

After performing the necessary compilation, feel free to utilize the following examples
for testing in the SLIME REPL:

	* (integrate '1 'x)

	* (integrate '(+ x 14) 'x 1 2)

If you have compiled the symbolic-integration-test-cases file, you can run the following
test functions:

	* (t1)
	* (t2)
	* .....
	* (t20) 



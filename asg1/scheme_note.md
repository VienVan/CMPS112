01/16/2018
**(read)** built-in function of Scheme - reads in an entire list, defined recursively
**Program 1**
- Everything written in quotes is written literally
- Italics indicate nonterminal symbols and token classes
- A line includes '(' + Line number | Label |
- Line# = car
- Statement / Label = cadr
- statement = caddr
- No need to worry about complex numbers or anomalies in Arithmetics
- Use *label* to indicate global identifiers
- e.g: string->integer is a function that converts strings to interget (naming convention)
- run checksource on your files
- Have to check to see if the program is null
	(not (null? prog))
		(interpret line (car prog))
		(interpret (cdr prog))
- Should have trivial functions written (like getlabel and getstatement)
- getlabel: if it takes a line as an argument, if the cdr is null, return null
	if cdr is not null, check to see if cadr is a symbol, if yes, return symbol, if not, check the caddr (?)
- how to interpret statement: every statement includes a keyword (goto, let, if, etc), followed by ...?
- getstatemnt: returns null if no statement, if not, returns a list, then look up the keyword from the keyword table
- print function itself will take its cdr as an argument
	if string, print string, if not, call evalexpression
		an expression always contains an operator as it's first thing (cdr)
- interpreter should not detect infinite loop
- don't have to worry about error messages, unless there's a goto label that doesn't exist


- Q: Difference between .bashrc vs .bash_profile?
- A: Don't care.

car - line number, cadr - keyword,

Apple doesn't like Linux because if you need to modify Linux, have to make it public.

**scheme**
- Your program points at the line
- everything in parentheses are lists
- probably want to draw a bunch of diagrams about the list structures
- Program starts at first line
	check if it's not null
	trivial program = () (line number = 0)
	to interpret program, first interpret a Statement
	car of program grabs us the line number
	cadr grabs the statement
	interpret program calls interpret program calls tail-recursive
	have to call evaluate expression on the cadr (cdr?) (not clear, gotta ask professor)
- when we call interpret program recursively, what do we want to do?
	we want to call it on the cdr of the current program, or on the value returned by the lookup from the label table
- when encountering +, \*, /,, etc. have to call evaluate expression
- find out what a tailcall does
- *debug*
	REPL

01/18/2018

(1)
(2 l)
(3 (set x (+ a b)))
(4 (goto l))

how to create label table?
- should be a tail recursive functions
- take the car of the the Program
- cadr of the list
- symbol? pair? null?
- if cdr is not null, check to see what's in the cadr?
- want to write trivial functions that have better names than simply linking down the list
- if the line has a second thing in it, that second thing will be either a symbol? -> label, or pair? -> Statement
- insert label into label table
- 2 things main program has to do:
	1. make single pass creating a label table
	2. then it has to interpret a program (interp-prog)
		- check to see if there's a statment on that particular line
		- got to do:
			1. interp-statement - return a value, cehck waht that value is, then call interp program
				- look up Keyword (always the fist thing in the statement)
				- from that, call interp-x
				- where x could be (dim, let, goto, etc.)
				- put these in a separte table or into a function table
				- value in the keyword table simply going to be pointers to function
				- have to write one function for each one of the possible keyword
			2. call interp-program
				- cdr prog label?

- need to write a funcion called evalexpression
	- what is an expression?
		- defined as a binop or unop
		- what kind of argument might you have in an expression?
			- number
			- memory reference
				- could be just a symbol or a pair
				- if pair, could be (array expr) | (function ex)
				- if you have a pair, pair always has a car, could be variable or function
				- one thing you could do is look up function in the function table, if found, it's a function, if not, then it's a variable (Array)
				- memory can be expanded as a symbol or a symbol followed by an expression
				- array or function is am ambiguity in the language
				- if the car of a memory reference is a function, simply pass the cdr into that function
				- if it is a variable (array expression), verify that
				- symbol variable contains a number
- need to check if the program's car is null
	- use (when (not (null? prog)))
	- cond is a way to use else if else if

- (map f l) -> apply f to each element of l and return new list
- (+ x 0.0)

- store that value into the variable table, or memory is referencing an array
- if Statement
	- has exactly 3 arguments in
	- relop is in the function table

**listhash example**
- add in *statement-table*
- labels and variables are separate
- to put something in a hash table, use hash-set!
- to put something in a vector, use vector-set!
- functions and variables can be in the same table
- hash-for-each -> equivalent to hash.foreach(lambda)
	 - looks through the entire argument,

**symbols.scm**
- define
	- when it takes a word, then it's a symbol
	- when it takes a pair, then it's a function ?)
	- (,) causes something to be unquoted and evaluated
- quasi quote (`) -> quoted
	- quote the entire list, but if it's unquoted, then it's evaluated first
- single quote ('), quote the entire list
- lambda - functional constant, what a define does
	- (define (f x) (+ x 1)) = (define f (lambda (x) (+ x 1))
	- inline functional constant
- rem = % (given in pdf handout)
- symbol-put (put things in a symbol table) (defined in symbols.scm using hash-set!)
- symbol-get (get things from the symbol table) (using hash-ref)

- when you want to see if soemthing is a tail call or not, trace through the execution of the program to see wha's going on

*example of a tail-recursive function*
(define (len l)
	(define (m x)
		(if (null? m) x)
			( m (+ 1 x) (cdr x))))
	(m l 0)

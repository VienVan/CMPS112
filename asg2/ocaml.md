# Write DC calculator based on dc in unix
- c clears the stack
- f pritns the entire stack
- l and s loads and stores elements into registers
- don't use anything in the library
- tag indicates integer (1) or pointer (0) (last bit)
- 6328 + 432 -> use same algorithm in elementary school
	carry over
	how to represent this? process lower digit to higher digit
	8 -> 2 -> 3 -> 6 (list, one digit per node, head of the list is the lower digit)??
	2 -> 3 -> 4 (list)
- write tail recursive function
- implement add and subtract first (easiest)
- print higher end of the list to the lower end of the list
- dc prints out backslash and new line for very large numbers
- first letter of an identifier is significant
- name of module or construtor has to be capitalized
- constructor just creates an object
- "include Bigint" -> #include or import
- token is a thing that's returned by the Scanner
- Bigint.bigint -> Bigint = module name -> something in Bigint (Bigint::bigint)

- lexbuf -> buffer where a the scanner keeps the characters that are scanned in
- "let a = 6 in a + b" -> expression

- without parentheses, functions are applied from left to right  
- raise = throw (exception handling)

- beta reduction -> define one function interm of another without specifying last argument

ddeclare type
- curried fucntions (look up curried functions)
let f (x:int) (y:int)  = x + y

- want to keep the Stack unchanged
- "!" dereferences things

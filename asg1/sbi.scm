#!/Applications/Racket/bin/mzscheme -qr
;#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; 1634132 (vhvan) - Vien Van : sbi.scm
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;; TODO: define let, goto, let, if, fib, pi, e, etc/

;; professor wrote these functions
(define *stderr* (current-error-port))

(define *run-file*
	(let-values
		(((dirpath basepath root?)
			(split-path (find-system-path 'run-file))))
		(path->string basepath))
)

(define (die list)
	(for-each (lambda (item) (display item *stderr*)) list)
	(newline *stderr*)
	(exit 1)
)

(define (usage-exit)
	(die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
	(let ((inputfile (open-input-file filename)))
		 (if (not (input-port? inputfile))
			 (die `(,*run-file* ": " ,filename ": open failed"))
			 (let ((program (read inputfile)))
				  (close-input-port inputfile)
						 program))))

;;;;;;;;;;;;;;;;;;;; END MACKEY'S FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helper functions
(define list-length
	(lambda (l)
		(cond	((null? l) 0)
		(#t (+ 1 (list-length (cdr l)))))
	)
)

; evaluate expression
; if symbol, return symbol
; if pair, look up car of pair in function table,
; then apply the eval-expr to the cdr (print string, evaluate operators, etc.)
; TODO: evaluate symbol
(define (eval-expr expr)
	; (printf "eval-expr exp: ~a~n" expr)
	(cond 	((number? expr) (+ expr 0.0))
			((hash-has-key? *variable-table* expr)
				; (printf "inside hash has key: ~a~n" expr)
				(get-var expr))
			((symbol? expr) expr)
			((string? expr) expr)
			((pair? expr)
				(if (eqv? (car expr) `dim)
					(apply (get-function (car expr)) (cdr expr))
					(apply (get-function (car expr)) (map eval-expr (cdr expr)) )
				)

			)
			(else #f)
	)
)

(define (get-statement line)
	; (printf "Length of line: ~a~n" (list-length line))
	(cond ((eqv? (list-length line) 2) (cdr line))  ;if the line list has 2 elements, Mackey said the statement is the cdr
		  ((eqv? (list-length line) 3) (cddr line)) ;if line has 3 elements, statement is the cddr
		  (else line)							;return line number if it's 1
	)
)

;;;;;;;;;;;;;;;;;;;;;; END HELPER FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;  VARIABLES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *variable-table* (make-hash))

(define (get-var key)
		(hash-ref *variable-table* key))

(define (put-var! key value)
		(hash-set! *variable-table* key value))

; init function table
(for-each
	(lambda (pair)
			(put-var! (car pair) (cadr pair)))
	`(
		(pi 3.141592653589793238462643383279502884197169399)
		(e 2.718281828459045235360287471352662497757247093)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;  END VARIABLES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;  FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;function table
(define *function-table* (make-hash))

;function table getter
(define (get-function key)
		(hash-ref *function-table* key))

;function table setter
(define (put-function! key value)
		(hash-set! *function-table* key value))

(define (let_ var . expr)
	(put-var! var (eval-expr (car expr)))
)

; modify print to take more than 1 arguments
; TODO handle case when no argument is given to print
(define (print_ expr . optional)
	(when (not (null? expr))
		(display expr)
		(when (not (null? optional))
			(display (car optional))
		)
		(newline)
	)
)

(define (dim_ expr)
	(put-var! (car expr) (make-vector (car (cdr expr))))
	(vector-set! (get-var (car expr)) 5 10)
	(display (get-var (car expr)))
	(newline)
)
; init function table
(for-each
	(lambda (pair)
			(put-function! (car pair) (cadr pair)))
	`(
		(let ,let_)
		(print ,print_)
		(dim ,dim_)
		(+ ,+)
		(- ,-)
		(* ,*)
		(/ ,(lambda (x y) (floor (/ x y))))
		(atan ,atan)
		(cos ,cos)
		(tan ,tan)
		(abs ,abs)
		(^ ,expt)
		(sin ,sin)
		(exp ,exp)
		(log10_2 0.301029995663981195213738894724493026768189881)
		(log10   ,(lambda (x) (/ (log x) (log 10.0))))
		(log   ,(lambda (x) (/ (log x) (log 10.0))))
		(sqrt ,sqrt)

	)
)

;;;;;;;;;;;;;;;;;;;;;;;;; END FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; interpret program by line
(define (interp-prog program)
	(when (not (null? program))
		(let ((line (car program)))
			; (printf "line length: ~a~n" (list-length line))
			; (printf "line: ~a~n" line)
			(let ((statement (get-statement line)))
				; (printf "statement: ~a~n" statement)
				; (printf "statement list length: ~a~n" (list-length statement))
				; (printf "car statement: ~a~n~n" (car statement))
				(let ((expr (car statement)))
					(eval-expr expr)
				)
			)
		)
		(interp-prog (cdr program))
	)
)

(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* ((sbprogfile (car arglist))
			   (program (readlist-from-inputfile sbprogfile)))
			  (interp-prog program)
		)
	)
)

(main (vector->list (current-command-line-arguments)))

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
;; TODO: define goto, if, fib
;; TODO: ARRAYS (DIM) (12-let)
;; TODO: 20-goto
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

; TODO: label is printing out in order, but its still printing out the rest of the program
(define (eval-expr expr)
	; (printf "eval-expr exp: ~a~n" expr)
	(cond 	((number? expr) expr)
			((hash-has-key? *variable-table* expr)
				(get-var expr))
			((hash-has-key? *label-table* expr)
				(goto_ expr)
			)
			((symbol? expr) expr)
			((string? expr) expr)
			((pair? expr)
				(cond 	((eqv? (car expr) `dim)
							(dim_ (cadr expr))
						)
						((eqv? (car expr) `goto)
							; (printf "inside goto: ~a~n" (cadr expr))
							; (eval-expr (get-label (cadr expr)))
							; (printf "goto: ~a~n" (cadr expr))
							; (printf "get-label: ~a~n" (get-label (cadr expr)))
							; (get-label (cadr expr))
							(goto_ (cadr expr))
						)
						((eqv? (car expr) `let)
							(let_ (cadr expr) (caddr expr))
						)
						((eqv? (car expr) `if)
							; (printf "if: ~a~n" (cadr expr))
							; (printf "if: ~a~n" (caddr expr))
							(if_ (cadr expr) (caddr expr))
						)
						((hash-has-key? *function-table* (car expr))
							(apply (get-function (car expr)) (map eval-expr (cdr expr)) )
						)
						((hash-has-key? *variable-table* (car expr))
							(if (vector? (get-var (car expr)))
								(vector-ref (get-var (car expr)) (eval-expr (cdr expr)) )
								(get-var (car expr))
							)

						)
						(else #f)
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

;;;;;;;;;;;;;;;;;;;;;;;;;  LABELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *label-table* (make-hash))

(define (get-label key)
		(hash-ref *label-table* key))

(define (put-label! key value)
		(hash-set! *label-table* key value))

(for-each
	(lambda (pair)
			(put-label! (car pair) (cadr pair)))
	`(
		(done, (newline))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;  END LABELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; handle the case where it's an Array
(define (let_ memory-ref expr)

	; (printf "memory-ref: ~a~n" memory-ref)
	(if (pair? memory-ref)
		(vector-set! (get-var (car memory-ref))
					 (eval-expr (cdr memory-ref))
					 (eval-expr expr)
		)
		(put-var! memory-ref (eval-expr expr))
	)

)

; modify print to take more than 1 arguments
; TODO handle case when no argument is given to print
(define (print_ . expr)
	(when (not (null? expr))
		(when (pair? expr)
			(display (car expr))
			(when (not (null? (cdr expr)))
				(display " ")
				(display (cadr expr))
			)
		(newline)
		)
	)
)

(define (dim_ array)
	(put-var! (car array) (make-vector (eval-expr (car (cdr array)))))
)

(define (if_ relop label)
	(when (eval-expr relop)
		(goto_ label)
	)
)
; ; TODO fix this function
(define (goto_ label)
	(when (not (null? (get-label label)))
		(interp-prog (get-label label))
	)
	; (if (not (null? label))
	; 	; ((display "found")
	; 	; (printf "label: ~a~n" label)
	; 	(printf "label: ~a~n" (eval-expr (get-label label)))
	; 	; (eval-expr (get-label label))
	; 	; (printf "get-label: ~a~n" (get-label label))
	; 	(display "Label not found")
	; )

)

(define (!= x y)
	(if (eqv? x y)
		#t
		#f
	)
)
; init function table
(for-each
	(lambda (pair)
			(put-function! (car pair) (cadr pair)))
	`(
		; (let 	,let_)
		(print 	,print_)
		; (dim 	,dim_)
		; (goto	,goto)
		(+ 		,+)
		(- 		,-)
		(* 		,*)
		(/ 		,(lambda (x y) (floor (/ (+ x 0.0) (+ y 0.0)))))
		(>		,>)
		(<		,<)
		(=		,=)
		(<>		,!=)
		(>=		,>=)
		(<=		,<=)
		(atan	,atan)
		(cos 	,cos)
		(acos 	,acos)
		(tan 	,tan)
		(abs 	,abs)
		(^ 		,expt)
		(sin 	,sin)
		(asin 	,asin)
		(exp 	,exp)
		(ceil 	,ceiling)
		(floor 	,floor)
		(round 	,round)
		(log10  ,(lambda (x) (/ (log x) (log 10.0))))
		(log   	,(lambda (x) (/ (log (+ x 0.0)) (log 10.0))))
		(sqrt 	,sqrt)

	)
)

;;;;;;;;;;;;;;;;;;;;;;;;; END FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO: store the value of label as the rest of the program
(define (get-labels program)
	(when (not (null? program))
		(when (not (null? (cdr program)))
			(let ((line (cdr program)))
				(when (eqv? (list-length (car line)) 3)
					(put-label! (cadr (car line)) line)
				)
			)
			(get-labels (cdr program))
		)
	)
)
; interpret program by line
(define (interp-prog program)
	(when (not (null? program))
		(let ((line (car program)))
			(let ((statement (get-statement line)))
				(let ((expr (car statement)))
					(eval-expr expr)
				)
			)
		)
		; TODO handle go to statement
		(interp-prog (cdr program))
	)
)

(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* 	((sbprogfile (car arglist)) (program (readlist-from-inputfile sbprogfile)))
				 (get-labels program)
				 (interp-prog program)
		)
	)
)

(main (vector->list (current-command-line-arguments)))

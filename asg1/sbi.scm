#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;#!/Applications/Racket/bin/mzscheme -qr
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

; evaluate numbers, expressions, and getting array values
(define (eval-expr expr)
	(cond 	((number? expr) (+ expr 0.0))
			((hash-has-key? *variable-table* expr)
				(get-var expr))
			((symbol? expr) expr)
			((string? expr) expr)
			((pair? expr)
				(cond
						((hash-has-key? *function-table* (car expr))
							(apply (get-function (car expr)) (map eval-expr (cdr expr)) )
						)
						((hash-has-key? *variable-table* (car expr))
							(if (vector? (get-var (car expr)))


								(vector-ref (get-var (car expr)) (exact-round(eval-expr (cdr expr))))
								(eval-expr (get-var (car expr)))
								; (display "not a vector")
							)

						)
						(else #f)
				)

			)
			(else #f)
	)
)


(define (get-statement line)
	(when (pair? line)
		(when (not (null? (cdr line)))
			(if (hash-has-key? *label-table* (car (cdr line)))
				(cdr (cdr line))
				(car (cdr line))
			)
		)
	)

)


;;;;;;;;;;;;;;;;;;;;;; END HELPER FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;  LABELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *label-table* (make-hash))

(define (get-label key)
		(hash-ref *label-table* key))

(define (put-label! key value)
		(hash-set! *label-table* key value))

;;;;;;;;;;;;;;;;;;;;;;;;;  END LABELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;  VARIABLES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make variable hash table
(define *variable-table* (make-hash))

; variable table getter
(define (get-var key)
		(hash-ref *variable-table* key))

; variable table setter
(define (put-var! key value)
		(hash-set! *variable-table* key value))

; init variable table with pi and e
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

; let takes in memory reference and expr
; if memory-ref is a pair, it's an array
; if not, it's a variable
(define (let_ memory-ref expr)
	(if (pair? memory-ref)
		(vector-set! (get-var (car memory-ref))
					 (exact-round (eval-expr (cdr memory-ref)))
					 (exact-round (eval-expr expr))
		)
		(put-var! memory-ref (eval-expr expr))
	)
	; (display memory-ref)
	; (newline)
	; (printf " memory-ref: ~a~n" (get-var memory-ref))

)

; modify print to take more than 1 arguments
; recursively print the cdr of print
; TODO: bug where it prints (expr) when there are two arguments, but normal when print 3
(define (print_ . expr)
	(display " ")
	(when (not (null? expr))
		(when (not (null? (car expr)))
			(display (eval-expr (caar expr)))
			(print_ (cdar expr))
		)
	)
)


; dim (array): create an array in a variable table with dimension given in array pair
; array -> (var expr)
(define (dim_ array)
	(put-var! (car array) (make-vector (exact-round (+ (eval-expr (car (cdr array))) 1)) )
	)
)

; if takes a boolean relative operation + label
; if boolean=t, goto label
(define (if_ relop label)
	(when (eval-expr relop)
		(goto_ label)
	)
)

; goto get the prog address stored in label table, run program
; this is running an infinity
(define (goto_ label)
		(interp-prog (get-label label))
)

; <> (not equal to)
(define (!= x y)
	(if (eqv? x y)
		#t
		#f
	)
)

(define (input_ memory)
	(put-var! `inputcount 0)
	(let ((input (read)))
		(cond 	((number? input)
					(put-var! (car memory) input)
					(put-var! `inputcount (+ (get-var `inputcount) 1))
					; (display "x: ")
					; (printf "get memory: ~a~n" (get-var (car memory)))
				)
				((eof-object? input)
				 (put-var! `inputcount -1)
				 )
				(else (
					(display "Invalid! Must be a Number")
					(newline)
					(input_ memory)
					))

		)

	)
	; (printf "inputcount: ~a~n" (get-var `inputcount))
	(when (not (null? (cdr memory)))
		(input_ (cdr memory))
	)


)

; init function table
(for-each
	(lambda (pair)
			(put-function! (car pair) (cadr pair)))
	`(
		(+ 		,+)
		(- 		,-)
		(* 		,*)
		(/     ,(lambda (x y)  (if (and (eq? x 1) (eq? y 1)) (/ (+ x 0.0) (+ y 0.0)) (/ x y))))
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
(define (interp-statement statement)
	; (display "inside interp statement")
	(when (not (null? statement))
		(when (pair? statement)
			; (printf "cdr statement: ~a~n" (cdr statement))
				(cond 	((eqv? (car statement) `print)
							(print_ (cdr statement))
							(newline)
						)
						((eqv? (car statement) `dim)
							(dim_ (cadr statement))
						)
						((eqv? (car statement) `goto)
							(goto_ (cadr statement))
							; (interp-statement (cadr statement))
						)
						((eqv? (car statement) `input)
							(input_ (cdr statement))
						)
						((eqv? (car statement) `let)
							(let_ (cadr statement) (caddr statement))
						)
						((eqv? (car statement) `if)
							(if_ (cadr statement) (caddr statement))
						)
						(else #f
							; (printf "interp-statement false: ~a~n" (car statement))
							(interp-statement (car statement))
							)
				)

				; (printf "memory-ref" (get-var memory-ref))
		)


	)
)

; get label store label as key and address of the rest of the program as value
(define (get-labels program)
	(when (not (null? program))
			(let ((line (car program)))
				(when (eqv? (list-length line) 2)
					(when (not (pair? (car (cdr line))))
						(put-label! (cadr line) program)
					)
				)
				(when (eqv? (list-length line) 3)
					(put-label! (car (cdr line)) program)
				)
			)
			(get-labels (cdr program))
		)

)

(define (interp-prog program)
	(when (not (null? program))

		(interp-statement (get-statement (car program)))
		(when (pair? (cdr program))
			(when (pair? (get-statement (car (cdr program))))
				(when (eqv? (car (get-statement (car (cdr program)))) `goto)
					(exit)
				)
			)
		)
		; (printf "car get-statemtn program: ~a~n" (get-statement (car (cdr program))))

		(interp-prog (cdr program))

	)
)

; driver function
(define (main arglist)
	(if (or (null? arglist) (not (null? (cdr arglist))))
		(usage-exit)
		(let* 	((sbprogfile (car arglist)) (program (readlist-from-inputfile sbprogfile)))
				 (get-labels program)
				 (interp-prog program)
		)
	)
)


; run interpreter
(main (vector->list (current-command-line-arguments)))

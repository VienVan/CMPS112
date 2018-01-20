#!/Applications/Racket/bin/mzscheme -qr
;#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
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
;; end professor written functions

;function table
(define *function-table* (make-hash))

;function table getter
(define (get-function key)
		(hash-ref *function-table* key))

;function table setter
(define (put-function! key value)
		(hash-set! *function-table* key value))

;init function table
(for-each
	(lambda (pair)
			(put-function! (car pair) (cadr pair)))
	`(
		(print ,print)
		(+ ,+)
		(- ,-)
		(* ,*)
		(/ ,/)
	))

; evaluate line

; evaluate expression
; if number, return number
; if symbol, return symbol
; if pair, look up car of pair in function table, then apply the eval-expr to the cdr (print string, evaluate operators, etc.)
; TODO: evaluate symbol
(define (eval-expr expr)
	(cond 	((number? expr) expr)
			((symbol? expr) expr)
			((string? expr) expr)
			((pair? expr)
				(apply (
					get-function (caar expr))
					(map eval-expr (cdar expr))
				)
			)
			(else #f)
	)
)

; interpret program by line
(define (interp-prog program)
	(when (not (null? program))
		(let ((second (cdar program)))
			(when (not (null? second))
				(eval-expr second)
				(newline)
			)
		)
		(interp-prog (cdr program))

	)
)
; label table
; (define *label-table* (make-hash))
;
; (define (label-put list)
; 	(when (not (null? list))
; 		(let ((first (cdr list)))
; 			(when (symbol? first)
; 				(hash-set! *label-table* first list)
; 			)
; 		)
; 		(label-put (cadr list))
; 	)
; )
; interpret program

; (define (write-program-by-line filename program)
; 	(printf "==================================================~n")
; 	(printf "~a: ~s~n" *run-file* filename)
; 	(printf "==================================================~n")
; 	(printf "(~n")
; 	(map (lambda (line) (printf "~s~n" line)) program)
; 	(printf ")~n"))

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

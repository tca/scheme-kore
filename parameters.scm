;; requires delimcc.scm

; Get the parameter to parameterize itself
(define-record-type parameterization
  (make-parameterization newv thunk)
  parameterization?
  (newv parameterization-newv)
  (thunk parameterization-thunk))

(define (make-parameter default)
  (let ((p (new-prompt)))
    (lambda args
      (cond
	((null? args)			; de-referencing
	  (if (prompt-set? p)
	    (shift p f (lambda (y) ((f y) y)))	; send a request
	    default))
	((null? (cdr args))
	  (cond
	    ((parameterization? (car args))
	      (dyn-bind p (parameterization-newv (car args))
		(parameterization-thunk (car args))))
	    ((prompt-set? p)		; `mutate' the dynvar, if was dyn-bound
	      (shift p f (lambda (y) ((f y) (car args)))))
	    (else (set! default (car args)))))
	(else
	  (error 'make-parameter "bad args~a~n" args))))))

(define (dyn-bind p val thunk)
  ((push-prompt p
     (let ((r (thunk)))
       (lambda (y) r)))
    val))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((p v)) e1 e2 ...)
      (p (make-parameterization v (lambda () e1 e2 ...))))))

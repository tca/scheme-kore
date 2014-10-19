(define (compile-pattern p)
  (cond 
   ((equal? '_ p) (wildcard-pat))
   ((symbol? p) (var-pat p '_))
   ((list? p)
    (case (car p)
      ((==) (apply equal?-pat (cdr p)))
      ((?) (apply pred-pat (cdr p)))
      ((or) (apply or-pat (cdr p)))
      ((and)(apply and-pat (cdr p)))
      ((not) (apply not-pat (cdr p)))
      ((cons)(apply cons-pat (cdr p)))
      ((list) (apply list-pat (cdr p)))
      ((@) (apply var-pat (cdr p)))
      ((quote) (equal?-pat p))
     ;; ((quasiquote) ...)
      (else (apply list-pat p))))
   ((pair? p) (cons-pat (car p) (cdr p)))
   (else (error "invalid pattern" p))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATTERN COMBINATORS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id-pat thing) (lambda (obj env) thing))
(define (complement-pat p1) (lambda (p f) (p1 f p)))
(define (conj-pat p1 p2) (lambda (p f) (p1 (p2 p f) f)))

(define (disj-pat p1 p2)
  (lambda (p f)
    (let* ((*e* '()) (*o* '())
           (p1c (reflect (lambda (o e p f) (set! *o* o) (set! *e* e) ((p1 p f) o e))))
           (p2c (reflect (lambda (o e p f) ((p2 p f) *o* *e*)))))
      (p1c p (p2c p f)))))


(define (case-pat xform)
  (lambda (pass fail)
    (lambda (obj env)
      `(if ,(xform obj)
	   ,(pass obj env)
	   ,(fail obj env)))))

(define (reflect fn)
  (lambda (pass fail)
    (lambda (obj env)
      (fn obj env pass fail))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE PATTERNS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (wildcard-pat) (lambda (p f) p))
(define (fail-pat) (lambda (p f) f))

(define (not-pat sub-pattern)
  (complement-pat (compile-pattern sub-pattern)))

(define (and-pat . sub-patterns)
  (foldl conj-pat (wildcard-pat) (map compile-pattern sub-patterns)))

(define (or-pat . sub-patterns)
  (foldl disj-pat (fail-pat) (map compile-pattern sub-patterns)))

(define (pred-pat fn)
  (case-pat (lambda (obj) `((function ,fn) ,obj))))

(define (equal?-pat pat)
  (case-pat (lambda (obj) `(equal? ,pat ,obj))))

(define (var-pat name pattern)
  (conj-pat
   (lambda (pass fail)
     (lambda (obj env)
       (if (assoc name env)
	   `(if (equal? ,name ,obj)
		,(pass obj env)
		,(fail obj env))
	   `(let ,(when name `((,name ,obj)))
	      ,(pass name (cons (cons name obj) env))))))
   (compile-pattern pattern)))

(define (cons-pat car cdr)
  (let ((*o* '()))
    (foldl
     conj-pat
     (wildcard-pat)
     (list (case-pat (lambda (obj) `(pair? ,obj)))
           (reflect (lambda (o e p f) (set! *o* o) (p o e)))
           (reflect (lambda (o e p f) (p `(car ,o) e)))
           (var-pat (gensym) car)
           (reflect (lambda (o e p f) (p *o* e)))
           (reflect (lambda (o e p f) (p `(cdr ,o) e)))
           (var-pat (gensym) cdr)
           (reflect (lambda (o e p f) (p *o* e)))))))

(define (list-pat . sub-patterns)
  (compile-pattern 
   (foldr (lambda (c m) `(cons ,c ,m))
          '(== '())
          sub-patterns)))

;;;;;;;;;;;;;;;;
;; MAIN FORMS ;;
;;;;;;;;;;;;;;;;

(define-syntax match
  (er-macro-transformer
   (lambda (exp r c)
     (let ((sym (gensym)))
       `(let ((,sym ,(cadr exp)))
          ,((foldr (lambda (clause fail)
                     ((compile-pattern (car clause))
                      (id-pat `(begin . ,(cdr clause)))
                      fail))
                   (id-pat '(error "match fail"))
                   (cddr exp))
            sym '()))))))

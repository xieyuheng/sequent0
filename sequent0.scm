(define data-stack '())
(define return-stack '())
(define binding-stack '())
(define name-stack '())

(define-macro (push stack value)
  `(set! ,stack (cons ,value ,stack)))

(define (head->name head)
  (car head))

(define (head->type head)
  (cdr (cdr head)))

(define (compile-jo jo)
  (define (var? v)
    (and (symbol? v)
         (equal? ":" (substring (symbol->string v) 0 1))))
  (define (call? v)
    (and (symbol? v)
         (not (eq? ":" (substring (symbol->string v) 0 1)))))
  (define (limited-call? v)
    (and (list? v)
         (not (member '-> v))
         (not (eq? (car v) 'lambda))
         (not (member ': v))
         (symbol? (last v))
         (not (var? (last v)))))
  (define (limited-apply? v)
    (and (list? v)
         (not (member '-> v))
         (not (eq? (car v) 'lambda))
         (not (member ': v))
         (or (not (symbol? (last v)))
             (var? (last v)))))
  (define (arrow? v)
    (and (list? v)
         (member '-> v)))
  (define (lambda? v)
    (and (list? v)
         (eq? (car v) 'lambda)))
  (define (ex-bind? v)
    (and (list? v) (pair? v)
         (not (equal? (car v) (vector 'flower-barcket/as-im-bind)))
         (member ': v)))
  (define (im-bind? v)
    (and (list? v) (pair? v)
         (equal? (car v) (vector 'flower-barcket/as-im-bind))
         (member ': v)))
  (cond [(var? jo)
         (list 'pre-var jo)]
        [(call? jo)
         (list 'call jo)]
        [(limited-call? jo)
         (compile-limited-call jo)]
        [(limited-apply? jo)
         (compile-limited-apply jo)]
        [(arrow? jo)
         (compile-arrow jo)]
        [(lambda? jo)
         (compile-lambda jo)]
        [(ex-bind? jo)
         (compile-ex-bind jo)]
        [(im-bind? jo)
         (compile-im-bind (cdr jo))]))

(define (compile-ex-bind jo)
  (list 'pre-ex-bind
        (compile-jo (car (right-of ': jo)))
        (compile-jojo (left-of ': jo))))

(define (compile-im-bind jo)
  (list 'pre-im-bind
        (compile-jo (car (right-of ': jo)))
        (compile-jojo (left-of ': jo))))

(define (compile-jojo jojo)
  (map compile-jo jojo))

(define (compile-arrow arrow)
  (list 'pre-arrow
        (compile-jojo (left-of '-> arrow))
        (compile-jojo (right-of '-> arrow))))

(define (compile-type type)
  (define (arrow? s)
    (and (eq? (length s) 1)
         (list? (car s))
         (member '-> (car s))))
  (cond [(arrow? type)
         (compile-arrow (car type))]
        [else
         (list 'pre-jojo (compile-jojo type))]))

(define (compile-body body)
  (map compile-arrow body))

(define (compile-lambda lambda)
  (list 'pre-lambda
        (compile-type (car (cdr lambda)))
        (compile-body (cdr (cdr lambda)))))

(define (compile-limited-call l)
  (list 'limited-call
        (last l)
        (compile-jojo (drop-last l))))

(define (compile-limited-apply l)
  (define (var? v)
    (and (symbol? v)
         (equal? ":" (substring (symbol->string v) 0 1))))
  (define (arrow? v)
    (and (list? v)
         (member '-> v)))
  (define (lambda? v)
    (and (list? v)
         (eq? (car v) 'lambda)))
  (define last-jo (last l))
  (define function-jojo
    (cond [(or (var? last-jo)
               (arrow? last-jo)
               (lambda? last-jo))
           (list (compile-jo last-jo))]
          [else
           (compile-jojo last-jo)]))
  (list 'limited-apply
        function-jojo
        (compile-jojo (drop-last l))))

(define (flower-barcket/as-im-bind body)
  (flower-barcket
   (lambda (dl)
     (cons (vector 'flower-barcket/as-im-bind)
           dl))
   body))

(define print-define-flag #f)
(define (print-define+) (set! print-define-flag #t))
(define (print-define-) (set! print-define-flag #f))

(define-macro (define-jojo . body)
  `(define-jojo1 . ,(flower-barcket/as-im-bind body)))

(define-macro (define-jojo head . tail)
  `($define-jojo (quote ,head) (quote ,tail)))

(define ($define-jojo head tail)
  (define name (head->name head))
  (define meaning
    (list 'meaning-jojo
          (compile-type (head->type head))
          (compile-jojo tail)))
  (push name-stack
        (cons name meaning))
  (if print-define-flag
    (let ()
      (display "\n")
      (display "<define-jojo>\n")
      (display ":name: ") (display name) (display "\n")
      (display ":meaning:\n")
      (display meaning) (display "\n")
      (display "</define-jojo>\n")
      (display "\n"))))

(define-macro (define-function . body)
  `(define-function1 . ,(flower-barcket/as-im-bind body)))

(define-macro (define-function1 head . tail)
  `($define-function (quote ,head) (quote ,tail)))

(define ($define-function head tail)
  (define name (head->name head))
  (define meaning
    (list 'meaning-function
          (compile-type (head->type head))
          (compile-body tail)))
  (push name-stack
        (cons name meaning))
  (if print-define-flag
    (let ()
      (display "\n")
      (display "<define-function>\n")
      (display ":name: ") (display name) (display "\n")
      (display ":meaning:\n")
      (display meaning) (display "\n")
      (display "</define-function>\n")
      (display "\n"))))

(define-macro (define-type . body)
  `(define-type1 . ,(flower-barcket/as-im-bind body)))

(define-macro (define-type1 head . tail)
  `($define-type (quote ,head) (quote ,tail)))

(define ($define-type head tail)
  (define name (head->name head))
  (define data-name-list (map car tail))
  (define meaning
    (list 'meaning-type-cons
          (compile-type (head->type head))
          name
          data-name-list))
  (push name-stack
        (cons name meaning))
  (if print-define-flag
    (let ()
      (display "\n")
      (display "<define-type>\n")
      (display ":name: ") (display name) (display "\n")
      (display ":meaning:\n")
      (display meaning) (display "\n")
      (display "</define-type>\n")
      (display "\n")))
  (map (lambda (h)
         ($define-data h name))
    tail)
  (void))

(define ($define-data head type-name)
  (define name (head->name head))
  (define meaning
    (list 'meaning-data-cons
          (compile-type (head->type head))
          name
          type-name))
  (push name-stack
        (cons name meaning))
  (if print-define-flag
    (let ()
      (display "\n")
      (display "<define-data>\n")
      (display ":name: ") (display name) (display "\n")
      (display ":meaning:\n")
      (display meaning) (display "\n")
      (display "</define-data>\n")
      (display "\n"))))

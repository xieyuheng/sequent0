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
  (cond [(var? jo)                (list 'pre-var jo)]
        [(call? jo)               (list 'pre-call jo)]
        [(limited-call? jo)       (compile-limited-call jo)]
        [(limited-apply? jo)      (compile-limited-apply jo)]
        [(arrow? jo)              (compile-arrow jo)]
        [(lambda? jo)             (compile-lambda jo)]
        [(ex-bind? jo)            (compile-ex-bind jo)]
        [(im-bind? jo)            (compile-im-bind (cdr jo))]))

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
  (list 'pre-limited-call
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
  (list 'pre-limited-apply
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

(define id/counter 0)

(define (id/new n ls)
  (set! id/counter (+ 1 id/counter))
  (vector (cons n id/counter) ls))

(define (unique-copy/pre-jojo pjj s)
  (: pre-jojo scope -> {jojo scope})
  (match pjj
    [{} {{} s}]
    [(pj . r)
     (match (unique-copy/pre-jo pj s)
       [{j s1}
        (match (unique-copy/pre-jojo r s1)
          [{jj s2}
           {(cons j jj) s2}])])]))

(define (unique-copy/pre-type pt s)
  (case (car pt)
    ['pre-arrow (unique-copy/pre-arrow pt s)]
    [else (unique-copy/pre-jojo pt s)]))

(define (unique-copy/pre-body pb s)
  (match pb
    [{} {{} s}]
    [(pa . r)
     (match (unique-copy/pre-arrow pa s)
       [{a s1}
        (match (unique-copy/pre-body r s1)
          [{b s2}
           {(cons a b) s2}])])]))

(define (unique-copy/pre-jo pj s)
  (: pre-jo scope -> {jo scope})
  (case (car pjj)
    ['pre-var           (unique-copy/pre-var pj s)]
    ['pre-call          (unique-copy/pre-call pj s)]
    ['pre-limited-call  (unique-copy/pre-limited-call pj s)]
    ['pre-limited-apply (unique-copy/pre-limited-apply pj s)]
    ['pre-arrow         (unique-copy/pre-arrow pj s)]
    ['pre-lambda        (unique-copy/pre-lambda pj s)]
    ['pre-ex-bind       (unique-copy/pre-ex-bind pj s)]
    ['pre-im-bind       (unique-copy/pre-im-bind pj s)]))

(define (unique-copy/pre-var pv s)
  (match pv
    [{'pre-var n}
     (let ([found (assq n s)])
       (if found
         (let ([old-id (cdr found)])
           {{'var old-id} s})
         (let ([new-id (id/new n '())])
           {{'var new-id}
            (cons (cons n new-id) s)})))]))

(define (unique-copy/pre-call pc s)
  (match pc
    [{'pre-call n}
     {{'call n} s}]))

(define (unique-copy/pre-limited-call pc s)
  (match pc
    [{'pre-limited-call n pjj}
     (match (unique-copy/pre-jojo pjj s)
       [{jj s1}
        {{'limited-call n jj} s1}])]))

(define (unique-copy/pre-limited-apply pa s)
  (match pa
    [{'pre-limited-apply pj pjj}
     (match (unique-copy/pre-jo pj s)
       [[{j s1}]
        (match (unique-copy/pre-jojo pjj s1)
          [{jj s2}
           {{'limited-apply j jj} s2}])])]))

(define (unique-copy/pre-arrow pa s)
  (match pa
    [{'pre-arrow pjj1 pjj2}
     (match (unique-copy/pre-jojo pjj1 s)
       [[{jj1 s1}]
        (match (unique-copy/pre-jojo pjj2 s1)
          [{jj2 s2}
           {{'array jj1 jj2} s2}])])]))

(define (unique-copy/pre-lambda pl s)
  (match pl
    [{'pre-lambda pt pb}
     (match (unique-copy/pre-type pt s)
       [[{t s1}]
        (match (unique-copy/pre-body pb s1)
          [{b s2}
           {{'lambda t b} s2}])])]))

(define (unique-copy/pre-ex-bind pe s)
  (match pe
    [{'pre-ex-bind pj pvl}
     (match (unique-copy/pre-jo pj s)
       [[{j s1}]
        (match (unique-copy/pre-jojo pvl s1)
          [{vl s2}
           {{'ex-bind j vl} s2}])])]))

(define (unique-copy/pre-im-bind pi s)
  (match pi
    [{'pre-im-bind pj pvl}
     (match (unique-copy/pre-jo pj s)
       [[{j s1}]
        (match (unique-copy/pre-jojo pvl s1)
          [{vl s2}
           {{'im-bind j vl} s2}])])]))

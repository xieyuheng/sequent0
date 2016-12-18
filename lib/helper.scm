(define-macro (cat-one e)
  (cond [(and (pair? e)
              (string? (car e)))
         (let ([str (car e)]
               [args (cdr e)])
           `(format #t ,str . ,args))]
        [(and (pair? e)
              (not (string? (car e))))
         e]
        [else
         `(error 'cat-one)]))

(define-macro (cat . l)
  (if (null? l)
    `(void)
    (let* ([h (car l)]
           [r (cdr l)])
      `(let ()
         (cat-one ,h)
         (cat . ,r)))))

(define-macro (orz who . l)
  `(let ()
     (cat ("~%")
          ("<~a>~%" ,who)
          (cat . ,l)
          ("~%")
          ("</~a>~%" ,who)
          ("~%"))
     (error ,who "")))

(define-syntax if3
  (syntax-rules ()
    [(if3 [a ...]
          [b ...]
          [])
     (if (let () a ...)
       (let () b ...))]
    [(if3 [a ...]
          []
          [c ...])
     (if (not (let () a ...))
       (let () c ...))]
    [(if3 [a ...]
          [b ...]
          [c ...])
     (if (let () a ...)
       (let () b ...)
       (let () c ...))]))

(define-syntax type
  (syntax-rules ()
    [(type . body)
     (void)]))

(define-syntax :
  (syntax-rules ()
    [(: . body)
     (void)]))

(define-syntax test
  (syntax-rules ()
    [(test b1 b2)
     (if (equal? b1 b2)
       #t
       (let ()
         (cat ("~%")
              ("<begin-test-fail-report>~%")
              (":actual-form:~%"))
         (pretty-print (quote b1))
         (cat (":actual-value:~%"))
         (pretty-print b1)
         (cat (":expect-form:~%"))
         (pretty-print (quote b2))
         (cat (":expect-value:~%"))
         (pretty-print b2)
         (cat ("<test-fail-report-end>~%"))
         (orz 'test (">_<"))))]))

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (left-of s l)
  (: sexp list -> list)
  (cond [(equal? s (car l)) '()]
        [else (cons (car l) (left-of s (cdr l)))]))

(define (right-of s l)
  (: sexp list -> list)
  (cond [(equal? s (car l)) (cdr l)]
        [else (right-of s (cdr l))]))

(define (sublist l start end)
  (: list index index -> list)
  (cond [(and (eq? 0 start) (<= end 0)) '()]
        [(and (not (eq? 0 start)))
         (sublist (cdr l) (- start 1) (- end 1))]
        [(and (eq? 0 start) (not (eq? 0 end)))
         (cons (car l) (sublist (cdr l) 0 (- end 1)))]))

(define (genlist len)
  (: length -> list)
  (letrec ([recur
            (lambda (len counter)
              (cond [(eq? len counter) '()]
                    [else (cons counter
                                (recur len (+ 1 counter)))]))])
    (recur len 0)))

(define (substitute e p? l)
  (: element (element -> bool) (element ...) -> (element ...))
  (cond [(eq? '() l) '()]
        [(p? (car l)) (cons e (cdr l))]
        [else (cons (car l) (substitute e p? (cdr l)))]))

(define (list-every? p? l)
  (: (element -> bool) l -> bool)
  (not (member #f (map p? l))))

(define (list-any? p? l)
  (: (element -> bool) l -> bool)
  (member #t (map p? l)))

(define (last l)
  (cond [(eq? '() (cdr l))
         (car l)]
        [else
         (last (cdr l))]))

(define (drop-last l)
  (cond [(eq? '() (cdr l))
         '()]
        [else
         (cons (car l) (drop-last (cdr l)))]))

(define (list-sub l1 l2)
  (cond [(eq? l1 '())
         '()]
        [(eq? l1 l2)
         '()]
        [else
         (cons (car l1) (list-sub (cdr l1) l2))]))

(define (pair-list . l)
  (match l
    [{} '()]
    [{x} (orz 'pair-list
           ("meet uneven list with ending : ~a~%" x))]
    [(x y . z) (cons (cons x y)
                     (apply pair-list z))]))

(define (sexp->string s)
  (format #f "~a" s))

(define (find-char c s)
  (: char string -> (or curser #f))
  (find-char/curser c s 0))

(define (find-char/curser c s curser)
  (: char string curser -> (or curser #f))
  (if (>= curser (string-length s))
    #f
    (let ([c0 (substring s curser (+ 1 curser))])
      (if (equal? c c0)
        curser
        (find-char/curser c s (+ 1 curser))))))

(define (symbol-append . l)
  (: symbol ... -> symbol)
  (string->symbol
   (apply string-append
     (map symbol->string l))))

(define (symbol-car v)
  (string->symbol (substring (symbol->string v) 0 1)))

(define (symbol-cdr v)
  (let ([str (symbol->string v)])
    (string->symbol (substring str 1 (string-length str)))))

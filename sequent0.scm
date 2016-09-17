(define ns '())
(define ds '())
(define bs '())
(define rs '())
(define gs '())

(define-macro (push s v) `(set! ,s (cons ,v ,s)))
(define-macro (put s l)
  `(set! ,s (append ,l ,s)))

(define-macro (tos s)    (car s))
(define-macro (pop s)
  (let ([v (gensym "pop/v")])
    `(let ([,v (car ,s)])
       (set! ,s (cdr ,s))
       v)))
(define-macro (fetch s n)
  (let ([v (gensym "fetch/v")])
    `(let ([,v (take ,s n)])
       (set! ,s (drop ,s n))
       v)))

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
  (define (apply? v)
    (eq? v 'apply))
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
        [(apply? jo)              (list 'pre-apply)]
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
  (push ns (cons name meaning))
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
  (push ns (cons name meaning))
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
  (push ns (cons name meaning))
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
  (push ns (cons name meaning))
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
    ['pre-apply         (unique-copy/pre-apply pj s)]
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
           {{'var old-id 0} s})
         (let ([new-id (id/new n '())])
           {{'var new-id 0}
            (cons (cons n new-id) s)})))]))

(define (unique-copy/pre-call pc s)
  (match pc
    [{'pre-call n}
     {{'call n} s}]))

(define (unique-copy/pre-apply pa s)
  (match pa
    [{'pre-apply} {{'apply} s}]))

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
    [{'pre-ex-bind pj pjj}
     (match (unique-copy/pre-jo pj s)
       [[{j s1}]
        (match (unique-copy/pre-jojo pjj s1)
          [{jj s2}
           {{'ex-bind j jj} s2}])])]))

(define (unique-copy/pre-im-bind pi s)
  (match pi
    [{'pre-im-bind pj pjj}
     (match (unique-copy/pre-jo pj s)
       [[{j s1}]
        (match (unique-copy/pre-jojo pjj s1)
          [{jj s2}
           {{'im-bind j jj} s2}])])]))

(define (bs/commit)
  (define (recur bs0)
    (cond [(equal? '(commit-point) (car bs0))
           (set! bs (cdr bs0))]
          [else
           (let ([id (car (car bs0))]
                 [ls (cdr (car bs0))])
             (id/commit id ls)
             (recur (cdr bs0)))]))
  (recur bs))

(define (id/commit id ls)
  (: id ls -> id [with effect on id])
  (let ()
    (vector-set! id 1 (append ls (vector-ref id 1)))
    id))

(define (bs/extend v d)
  (: var data -> !)
  (match v
    [{'var id level}
     (let ([found/ls (assq id bs)])
       (if found/ls
         (set! bs (substitute `(,id . ((,level . ,d) . ,(cdr found/ls)))
                              (lambda (pair) (eq? (car pair) id))
                              bs))
         (push bs `(,id . ((,level . ,d))))))]))

(define (bs/extend-up v d)
  (: var data -> !)
  (match v
    [{'var id level}
     (let ([level (+ 1 level)]
           [found/ls (assq id bs)])
       (if found/ls
         (set! bs (substitute `(,id . ((,level . ,d) . ,(cdr found/ls)))
                              (lambda (pair) (eq? (car pair) id))
                              bs))
         (push bs `(,id . ((,level . ,d))))))]))

;; in compose/var & cut/var
;;   extend bs whenever meet a new var
;;   this helps commit

(define (bs/extend-new v d)
  (: var data -> !)
  (match v
    [{'var id level}
     (push bs `(,id . ((,level . ,d))))]))

;; (define (bs/extend-new v d)
;;   (: var data -> !)
;;   (match v
;;     [{'var id level}
;;      (let ([found/ls (assq id bs)])
;;        (if found/ls
;;          (void)
;;          (push bs `(,id . ()))))]))

(define (id->ls id)
  (vector-ref id 1))

(define (bs/find v)
  (: var -> (or data #f))
  (match v
    [{'var id level}
     (let* ([found/commit (assq level (id->ls id))])
       (if found/commit
         (cdr found/commit)
         (let* ([found/ls (assq id bs)]
                [found/bind
                 (if found/ls
                   (assq level (cdr found/ls))
                   #f)])
           (if found/bind
             (cdr found/bind)
             #f))))]))

(define (bs/walk d)
  (: data -> data)
  (match d
    [{'var id level}
     (let ([found (bs/find d)])
       (if found
         (bs/walk found)
         d))]
    [{__ e} d]))

(define (bs/deep d)
  (: data -> data)
  (let ([d (bs/walk d)])
    (match d
      ;; a var is fresh after bs/walk
      [{'cons n dl}             {'cons n (bs/deep-list dl)}]
      [{'bind d sd}             {'bind (bs/deep d) (bs/deep sd)}]
      [{'trunk t k i}           {'trunk t (bs/deep-trunky k) i}]
      [__                    d])))

(define (bs/deep-list dl)
  (map (lambda (x) (bs/deep x)) dl))

(define (bs/deep-trunky k)
  (vector-set!
    k 0
    (match (vector-ref k 0)
      [{'todo b dl} {'todo b (bs/deep-list dl)}]
      [{'done dl}   {'done (bs/deep-list dl)}])))

(define (bs/find-up v)
  (: var -> (or data #f))
  (match v
    [{'var id level}
     (let* ([level (+ 1 level)]
            [found/commit (assq level (id->ls id))])
       (if found/commit
         (cdr found/commit)
         (let* ([found/ls (assq id bs)]
                [found/bind
                 (if found/ls
                   (assq level (cdr found/ls))
                   #f)])
           (if found/bind
             (cdr found/bind)
             #f))))]))

(define (var/fresh? v)
  (: var -> bool)
  (equal? (bs/walk v)
          v))

(define (var/eq? v1 v2)
  (match {v1 v2}
    [{{'var id1 level1} {'var id2 level2}}
     (and (eq? id1 id2)
          (eq? level1 level2))]))

(define (occur-check/data v d)
  (: fresh-var data -> bool)
  (match (bs/deep d)
    [{'var id level} (not (var/eq? v d))]
    [{'cons n dl}    (occur-check/data-list v dl)]
    [{'bind d sd}    (occur-check/data-list v {d sd})]
    [{'trunk t k i}  (occur-check/trunk v d)]
    [__              #t]))

(define (occur-check/data-list v dl)
  (: fresh-var {data ...} -> bool)
  (match dl
    [{} #t]
    [(d . r)
     (if (occur-check/data v d)
       (occur-check/data-list v r)
       #f)]))

(define (occur-check/trunk v t)
  (: fresh-var trunk -> bool)
  (match t
    [{'trunk t k i}
     (match (vector-ref k 0)
       [{'todo b dl} (occur-check/data-list dl)]
       [{'done dl}   (occur-check/data-list dl)])]))

(define (gs/exit) (void))

(define (gs/next)
  (: -> bool)
  (match (tos gs)
    [{c ex end {dl1 dl2}}
     (ex)]))

(define (try-trunk t)
  (: trunk -> (or #f data))
  )

(define (cover)
  (: -> bool)
  (match (pop gs)
    [{c ex end {dl1 dl2}}
     (cond [(>= c (length dl1))
            (end)
            #t]
           [else
            (let ([d1 (list-ref dl1 c)]
                  [d2 (list-ref dl2 c)])
              (push {(+ 1 c) ex end {dl1 dl2}})
              (if (cover/data/data d1 d2)
                (gs/next)
                #f))])]))

(define (cover/data/data d1 d2)
  (: data data -> bool)
  ;; var -walk-> fresh-var
  (let ([d1 (bs/walk d1)]
        [d2 (bs/walk d2)])
    (match {d1 d2}
      ;; ignore the sub-data
      ;;   for it is used by top-level type-check
      [{{'bind d v} __} (cover/data/data d d2)]
      [{__ {'bind d v}} (cover/data/data d1 d)]
      ;; var is the hero
      ;; this should pass occur-check
      [{{'var id1 level1} {'var id2 level2}}
       (cond [(var/eq? d1 d2) #t] ;; no self-cover
             [else (cover/var/data d1 d2)])]
      [{{'var id level} __} (cover/var/data d1 d2)]
      [{__ {'var id level}} #f] ;; the only difference from unify/data/data
      ;; cons push gs
      [{{'cons n1 dl1} {'cons n2 dl2}}
       (cond [(eq? n1 n2)
              (push gs {0 cover gs/exit {dl1 dl2}})
              (gs/next)]
             [else #f])]
      ;; trunk is the tricky part
      ;;   semantic equal is used
      [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}} (cover/trunk/trunk d1 d2)]
      [{{'trunk t k i} __} (cover/trunk/data d1 d2)]
      [{__ {'trunk t k i}} (cover/data/trunk d1 d2)]
      ;; others use syntax equal
      [{__ __} (equal? d1 d2)])))

;; ;; the equal? of scheme can handle circle
;; (let ([p1 (cons 1 1)]
;;       [p2 (cons 1 1)])
;;   (set-cdr! p1 p1)
;;   (set-cdr! p2 p2)
;;   (list p1 p2 (equal? p1 p2)))
;; ;; => (#0=(1 . #0#) #1=(1 . #1#) #t)

(define (cover/var/data v d)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (if (occur-check/data v d)
    (bs/extend v d)
    #f))

(define (cover/trunk/data t d)
  (let ([result (try-trunk t)])
    (if result
      (cover/data/data result d)
      #f)))

(define (cover/data/trunk d t)
  (let ([result (try-trunk t)])
    (if result
      (cover/data/data d result)
      #f)))

(define (cover/trunk/trunk t1 t2)
  (let ([result1 (try-trunk t1)]
        [result2 (try-trunk t2)])
    (cond [result1 (cover/data/trunk result1 t2)]
          [result2 (cover/trunk/data t1 result2)]
          [else
           ;; when both fail to try-trunk
           ;;   still have chance to syntax equal
           (match {t1 t2}
             [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}}
              (match {(vector-ref k1 0) (vector-ref k2 0)}
                [{{'todo b1 dl1} {'todo b2 dl2}}
                 (cond [(equal? {t1 i1 b1} {t2 i2 b2})
                        (push gs {0 cover gs/exit {dl1 dl2}})
                        (gs/next)]
                       [else #f])])])])))

(define (unify)
  (: -> bool)
  (match (pop gs)
    [{c ex end {dl1 dl2}}
     (cond [(>= c (length dl1))
            (end)
            #t]
           [else
            (let ([d1 (list-ref dl1 c)]
                  [d2 (list-ref dl2 c)])
              (push {(+ 1 c) ex end {dl1 dl2}})
              (if (unify/data/data d1 d2)
                (gs/next)
                #f))])]))

(define (unify/data/data d1 d2)
  (: data data -> bool)
  ;; var -walk-> fresh-var
  (let ([d1 (bs/walk d1)]
        [d2 (bs/walk d2)])
    (match {d1 d2}
      ;; ignore the sub-data
      ;;   for it is used by top-level type-check
      [{{'bind d v} __} (unify/data/data d d2)]
      [{__ {'bind d v}} (unify/data/data d1 d)]
      ;; var is the hero
      ;; this should pass occur-check
      [{{'var id1 level1} {'var id2 level2}}
       (cond [(var/eq? d1 d2) #t] ;; no self-unify
             [else (unify/var/data d1 d2)])]
      [{{'var id level} __} (unify/var/data d1 d2)]
      [{__ {'var id level}} (unify/var/data d2 d1)]
      ;; cons push gs
      [{{'cons n1 dl1} {'cons n2 dl2}}
       (cond [(eq? n1 n2)
              (push gs {0 unify gs/exit {dl1 dl2}})
              (gs/next)]
             [else #f])]
      ;; trunk is the tricky part
      ;;   semantic equal is used
      [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}} (unify/trunk/trunk d1 d2)]
      [{{'trunk t k i} __} (unify/trunk/data d1 d2)]
      [{__ {'trunk t k i}} (unify/data/trunk d1 d2)]
      ;; others use syntax equal
      [{__ __} (equal? d1 d2)])))

;; ;; the equal? of scheme can handle circle
;; (let ([p1 (cons 1 1)]
;;       [p2 (cons 1 1)])
;;   (set-cdr! p1 p1)
;;   (set-cdr! p2 p2)
;;   (list p1 p2 (equal? p1 p2)))
;; ;; => (#0=(1 . #0#) #1=(1 . #1#) #t)

(define (unify/var/data v d)
  (: fresh-var data -> bool)
  ;; no consistent-check
  ;;   because we do not have infer
  (if (occur-check/data v d)
    (bs/extend v d)
    #f))

(define (unify/trunk/data t d)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data result d)
      #f)))

(define (unify/data/trunk d t)
  (let ([result (try-trunk t)])
    (if result
      (unify/data/data d result)
      #f)))

(define (unify/trunk/trunk t1 t2)
  (let ([result1 (try-trunk t1)]
        [result2 (try-trunk t2)])
    (cond [result1 (unify/data/trunk result1 t2)]
          [result2 (unify/trunk/data t1 result2)]
          [else
           ;; when both fail to try-trunk
           ;;   still have chance to syntax equal
           (match {t1 t2}
             [{{'trunk t1 k1 i1} {'trunk t2 k2 i2}}
              (match {(vector-ref k1 0) (vector-ref k2 0)}
                [{{'todo b1 dl1} {'todo b2 dl2}}
                 (cond [(equal? {t1 i1 b1} {t2 i2 b2})
                        (push gs {0 unify gs/exit {dl1 dl2}})
                        (gs/next)]
                       [else #f])])])])))

(define (rs/exit) (void))

(define (rs/next)
  (match (tos rs)
    [{c ex end jj}
     (ex)]))

(define (compose)
  (match (pop rs)
    [{c ex end jj}
     (cond [(>= c (length jj))
            (end)]
           [else
            (let ([j (list-ref jj c)])
              (push {(+ 1 c) ex end jj})
              (compose/jo (car j))
              (rs/next))])]))

(define (compose/jo j)
  (case (car j)
    ['var           (compose/var j)]
    ['call          (compose/call j)]
    ['apply         (compose/apply j)]
    ['ex-bind       (compose/ex-bind j)]
    ['im-bind       (compose/im-bind j)]
    [__             (push ds j)]))

(define (compose/jojo jj) (for-each compose/jo jj))

(define (compose/var j)
  (if (var/fresh? j)
    (bs/extend-new v))
  (let ([d (bs/deep j)])
    (push ds d)))

(define (type/input-number t)
  (match t
    [{'arrow ajj sjj}
     (let* ([dp ds]
            [dl (let ()
                  (compose/jojo ajj)
                  (ds/gather-right dp))])
       (length dl))]
    [jj
     0]))

(define (type/output-number t)
  (match t
    [{'arrow ajj sjj}
     (let* ([dp ds]
            [dl (let ()
                  (compose/jojo sjj)
                  (ds/gather-right dp))])
       (length dl))]
    [jj
     (let* ([dp ds]
            [dl (let ()
                  (compose/jojo jj)
                  (ds/gather-right dp))])
       (length dl))]))

(define (compose/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'compose/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type-cons pt n nl}
            (let ([len (type/input-number (unique-copy/pre-type pt '()))])
              (push ds {'cons n (fetch ds len)}))]
           [{'meaning-data-cons pt n n0}
            (let ([len (type/input-number (unique-copy/pre-type pt '()))])
              (push ds {'cons n (fetch ds len)}))]
           [{'meaning-jojo pt pjj}
            (push rs {0 compose rs/next (unique-copy/pre-jojo pjj '())})]
           [{'meaning-function pt pb}
            (compose/function (unique-copy/pre-type pt '())
                              (unique-copy/pre-body pb '()))])))]))

(define (compose/function t b)
  ;; note that
  ;;   when create-trunk-list
  ;;   it needs to know the type to get input-number & output-number
  (let ([sjj (compose/try-body b)])
    (if sjj
      (push rs {0 compose rs/next sjj})
      (let ([dl (fetch ds (type/input-number t))])
        (put ds (create-trunk-list t b dl))))))

(define (compose/try-body b)
  (: body -> (or #f sjj))
  ;; return #f on fail with undo
  (match b
    [{} #f]
    [({'arrow ajj sjj} . r)
     (let ([ds0 ds]
           [gs0 gs]
           [bs0 bs])
       (push rs {0 compose rs/exit ajj})
       (rs/next)
       (push bs '(commit-point))
       (push gs {0 cover bs/commit (ds/gather ds0)})
       (if (gs/next)
         sjj
         (begin
           (set! ds ds0)
           (set! gs gs0)
           (set! bs bs0)
           (compose/try-body r))))]))

(define (create-trunk-list t b dl)
  (let ([k (vector {'todo b dl})])
    (reverse
     (map (lambda (i) {'trunk t k i})
       (genlist
        (type/output-number
         (unique-copy/pre-type pt)))))))

(define (gather-jojo jj)
  (let ([dp ds])
    (compose/jojo jj)
    (ds/gather-right dp)))

(define (compose/apply j)
  (match (bs/walk (pop ds))
    [{'lambda t b}
     (compose/function t b)]
    [__ (orz 'compose/apply
          ("can not handle jo : ~a~%" j))]))

(define (compose/ex-bind j)
  (match j
    [{'ex-bind j vl}
     (let* ([dp ds]
            [dl (let ()
                  (compose/jo j)
                  (ds/gather-right dp))]
            [d (car dl)])
       (if (not (eq? (length dl) 1))
         (orz 'compose/ex-bind
           ("jo should return one data~%")
           ("but this jo does not : ~a~%" j))
         (for-each (lambda (v)
                     (bs/extend-up v d)
                     (push ds {'bind d v}))
                   vl)))]))

(define (compose/im-bind j)
  (match j
    [{'im-bind j vl}
     (let* ([dp ds]
            [dl (let ()
                  (compose/jo j)
                  (ds/gather-right dp))]
            [d (car dl)])
       (if (not (eq? (length dl) 1))
         (orz 'compose/im-bind
           ("jo should return one data~%")
           ("but this jo does not : ~a~%" j))
         (for-each (lambda (v)
                     (bs/extend-up v d))
                   vl)))]))

(define (cut)
  (match (pop rs)
    [{c ex end jj}
     (cond [(>= c (length jj))
            (end)]
           [else
            (let ([j (list-ref jj c)])
              (push {(+ 1 c) ex end jj})
              (cut/jo (car j))
              (rs/next))])]))

(define (cut/jo j)
  (case (car j)
    ['var           (cut/var j)]
    ['call          (cut/call j)]
    ['apply         (cut/apply j)]
    ['arrow         (cut/arrow j)]
    ['lambda        (cut/lambda j)]
    ['ex-bind       (cut/ex-bind j)]
    ['im-bind       (cut/im-bind j)]))

(define (cut/var j)
  (if (var/fresh? j)
    (bs/extend-new v))
  (let ([d (bs/deep j)])
    (let ([found-d (bs/find-up j)])
      (if found-d
        (push ds found-d)
        (match j
          [{'var id level}
           {'var id (+ 1 level)}])))))

(define (cut/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'cut/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type-cons pt n nl}
            (cut/type (unique-copy/pre-type pt))]
           [{'meaning-data-cons pt n n0}
            (cut/type (unique-copy/pre-type pt))]
           [{'meaning-jojo pt pjj}
            (cut/type (unique-copy/pre-type pt))]
           [{'meaning-function pt pb}
            (cut/type (unique-copy/pre-type pt))])))]))

(define (cut/type t)
  (match t
    [{'arrow ajj sjj}
     (cut/try-arrow t)]
    [jj
     (compose/jojo jj)]))

(define (cut/try-arrow a)
  (: arrow -> !)
  (match a
    [{'arrow ajj sjj}
     (let ([ds0 ds])
       (push rs {0 compose rs/exit ajj})
       (rs/next)
       (push bs '(commit-point))
       (push gs {0 cover bs/commit (ds/gather ds0)})
       (if (gs/next)
         (compose/jojo sjj)
         (orz 'cut/type
           ("fail on cover~%"))))]))

(define (cut/apply j)
  (match (bs/walk (pop ds))
    [{'arrow ajj sjj}
     (cut/type {'arrow ajj sjj})]
    [__ (orz 'cut/apply
          ("can not handle jo : ~a~%" j))]))

(define (cut/arrow j)
  (orz 'cut/arrow
    ("can not handle arrow as jo that is not in type~%")
    ("jo : ~a~%" j)))

(define (cut/lambda j)
  (match j
    [{'lambda {'arrow ajj sjj} b}
     (push ds {'arrow ajj sjj})]
    [__
     (orz 'cut/lambda
       ("can not handle jo : ~a~%" j)
       ("for it is meaning less to write a lambda without local-vars~%"))]))

(define (cut/ex-bind j)
  (orz 'cut/ex-bind
    ("can not handle ex-bind as jo that is not in type~%")
    ("jo : ~a~%" j)))

(define (cut/im-bind j)
  (orz 'cut/im-bind
    ("can not handle im-bind as jo that is not in type~%")
    ("jo : ~a~%" j)))

(define (ds/gather dp)
  (: ds-pointer -> {dl1 dl2})
  (let* ([dl1 (list-sub ds dp)]
         [dl2 (fetch ds (length dl1))])
    (set! ds (drop (+ (length dl1) (length dl1))))
    (list dl1 dl2)))

(define (ds/gather-right dp)
  (: ds-pointer -> dl)
  (let ([dl (list-sub ds dp)])
    (set! ds (drop (length dl)))
    dl))

(define (app ))

(define (type-check ))

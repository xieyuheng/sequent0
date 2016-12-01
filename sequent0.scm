(: syntax
   var                :name
   fvar               ::name
   bind               %:name
   call               name
   arrow              (-> [syntax ...] [syntax ...])
   lambda             (lambda arrow arrow ...)
   apply              @)

(: jo
   var                name
   bind               name
   call               name
   arrow              {var ...} {fvar ...} {jo ...} {jo ...}
   lambda             arrow {arrow ...}
   apply)

(: data
   uni-var            id level
   uni-bind           uni-var data
   cons               name {data ...}
   uni-arrow          {var ...} {(fvar . uni-var) ...} {jo ...} {jo ...}
   uni-lambda         uni-arrow {uni-arrow ...}
   trunk              uni-arrow (vector trunky) index)

(: id                 (vector (name . counter) ls))
(: ls                 {(level . data) ...})
;; vector thus unique
;; ls (level binding-list) in id for commit

(: trunky
   todo               {uni-arrow ...} {data ...}
   done               {data ...})

(define-macro (push s v) `(set! ,s (cons ,v ,s)))
(define-macro (push-list s l)
  `(set! ,s (append ,l ,s)))

(define (tos s) (car s))
(define-macro (pop s)
  (let ([v (gensym "pop/v")])
    `(let ([,v (car ,s)])
       (set! ,s (cdr ,s))
       ,v)))
(define-macro (pop-list s n)
  (let ([v (gensym "fetch/v")])
    `(let ([,v (take ,s ,n)])
       (set! ,s (drop ,s ,n))
       ,v)))

;; name-stack
(define ns '())
(: ns {(name . meaning) ...})

;; data-stack
(define ds '())
(: ds {data ...})

(define (call-with-output-to-new-ds f)
  (: function -> new-ds)
  (let ([ds-backup ds])
    (set! ds '())
    (f)
    (let ([new-ds ds])
      (set! ds ds-backup)
      new-ds)))

;; binding-stack
(define bs '())
(: bs {(id . ls) ...})

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

;; not using ><><><
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
    [__ d]))

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

;; return-stack
(define rs '())
(define (rs/exit) (void))
(define (rs/next)
  (match (tos rs)
    [{c ex end jj}
     (ex)]))
(define rsp-proto
  (new-struct
   (pair-list
    'c      0
    'ex     '(explainer)
    'end    rs/exit
    'vrc    '(var record)
    'jj     '(jojo))))

;; goal-stack
;;   binding-stack is to record solution of equations in goal-stack
(define gs '())
(define (gs/exit) (void))
(define (gs/next)
  (: -> bool)
  (match (tos gs)
    [{c ex end {dl1 dl2}}
     (ex)]))
(define gsp-proto
  (new-struct
   (pair-list
    'c      0
    'ex     '(explainer)
    'end    gs/exit
    'dl+    '(data-list)
    'dl-    '(data-list))))

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

(define (try-trunk t)
  (: trunk -> (or #f data))
  (match t
    [{'trunk t k i}
     (match (vector-ref k 0)
       [{'done dl} (list-ref dl i)]
       [{'todo b dl}
        (push-list ds dl)
        (compose/function t b)
        (let ([result (pop ds)])
          (cond [(equal? result t) #f]
                [else result]))])]))

(define id/counter 0)

;; (define (id/new n ls)
;;   (set! id/counter (+ 1 id/counter))
;;   (vector (cons n id/counter) ls))

(define (id/new n)
  (set! id/counter (+ 1 id/counter))
  (vector (cons n id/counter) '()))

(define (vl->vrc vl)
  (map (lambda (v)
         (match v
           [{'var n}
            {'uni-var (id/new n) 0}]))
    vl))

(define (v->uv v)
  (match v
    [{'var n}
     (let ([rsp (tos rs)]
           [found (assq n (^ rsp 'vrc))])
       (if found
         (cdr found)
         (orz 'v->uv
           ("can not find name : ~a~%" n)
           ("rsp var record : ~a~%" (^ rsp 'vrc)))))]))

(define (cover)
  (: -> bool)
  (let* ([gsp (pop gs)]
         [c   (^ gsp 'c)]
         [ex  (^ gsp 'ex)]
         [end (^ gsp 'end)]
         [dl1 (^ gsp 'dl+)]
         [dl2 (^ gsp 'dl-)])
    (if3 [(>= c (length dl1))]
         [(end)
          #t]
         [(push gs (% gsp 'c (+ 1 c)))
          (if (cover/data/data (list-ref dl1 c)
                               (list-ref dl2 c))
            (gs/next)
            #f)])))

(define (cover)
  (: -> bool)
  (let* ([gsp (pop gs)]
         [c   (^ gsp 'c)]
         [ex  (^ gsp 'ex)]
         [end (^ gsp 'end)]
         [dl1 (^ gsp 'dl+)]
         [dl2 (^ gsp 'dl-)])
    (cond [(>= c (length dl1))
           (end)
           #t]
          [else
           (let ([d1 (list-ref dl1 c)]
                 [d2 (list-ref dl2 c)])
             (push gs (% gsp
                         'c (+ 1 c)))
             (if (cover/data/data d1 d2)
               (gs/next)
               #f))])))

;; (define gsp-proto
;;   (list
;;    (cons 'c   '())
;;    (cons 'ex  '())
;;    (cons 'end '())
;;    (cons 'dl1 '())
;;    (cons 'dl2 '())))

;; (@ 'copy gsp-proto
;;    'c 0
;;    'ex
;;    'end
;;    'dl1
;;    'dl2)

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
              (push gs (% gsp-proto
                          'ex cover
                          'end gs/exit
                          'dl+ dl1
                          'dl- dl2))
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
                        (push gs (% gsp-proto
                                    'ex cover
                                    'end gs/exit
                                    'dl+ dl1
                                    'dl- dl2))
                        (gs/next)]
                       [else #f])])])])))

(define (unify)
  (: -> bool)
  (let* ([gsp (pop gs)]
         [c   (^ gsp 'c)]
         [ex  (^ gsp 'ex)]
         [end (^ gsp 'end)]
         [dl1 (^ gsp 'dl+)]
         [dl2 (^ gsp 'dl-)])
    (if3 [(>= c (length dl1))]
         [(end)
          #t]
         [(push gs (% gsp 'c (+ 1 c)))
          (if (unify/data/data (list-ref dl1 c)
                               (list-ref dl2 c))
            (gs/next)
            #f)])))

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
              (push gs (% gsp-proto
                          'ex  unify
                          'end gs/exit
                          'dl+ dl1
                          'dl- dl2))
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
                        (push gs (% gsp-proto
                                    'ex unify
                                    'end gs/exit
                                    'dl+ dl1
                                    'dl- dl2))
                        (gs/next)]
                       [else #f])])])])))

(define (compose)
  (let* ([rsp (pop rs)]
         [c   (^ rsp 'c)]
         [ex  (^ rsp 'ex)]
         [end (^ rsp 'end)]
         [jj  (^ rsp 'jj)])
    (if3 [(>= c (length jj))]
         [(end)]
         [(push rs (% rsp 'c (+ 1 c)))
          (compose/jo (list-ref jj c))
          (rs/next)])))

(define (compose/jo j)
  (case (car j)
    ['var           (compose/var j)]
    ['bind          (compose/bind j)]
    ['call          (compose/call j)]
    ['apply         (compose/apply j)]
    [__             (push ds j)]))

(define (compose/var j)
  ;; (if (var/fresh? j)
  ;;   (bs/extend-new j))
  (let* ([uv (v->uv j)]
         [d (bs/deep uv)])
    (push ds d)))

(define (type/input-number t)
  (match t
    [{'arrow ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda () (for-each compose/jo ajj))))]))

(define (type/output-number t)
  (match t
    [{'arrow ajj sjj}
     (length (call-with-output-to-new-ds
              (lambda () (for-each compose/jo sjj))))]))

(define (compose/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'compose/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type pt n nl}
            (let ([len (type/input-number pt)])
              (push ds {'cons n (pop-list ds len)}))]
           [{'meaning-data pt n n0}
            (let ([len (type/input-number pt)])
              (push ds {'cons n (pop-list ds len)}))]
           [{'meaning-lambda pt pb}
            (compose/function pt pb)])))]))

(define (compose/function t b)
  ;; note that
  ;;   when create-trunk-list
  ;;   it needs to know the type to get input-number & output-number
  (let ([sjj (compose/try-body b)])
    (if sjj
      (push rs (% rsp-proto
                  'ex   compose
                  'end  rs/next
                  'jj   sjj))
      ;; no need to call (rs/next) here
      (let ([dl (pop-list ds (type/input-number t))])
        (push-list ds (create-trunk-list t b dl))))))

(define (compose/try-body b)
  (: body -> (or #f sjj))
  ;; return #f on fail
  ;; return sjj on success with commit
  (match b
    [{} #f]
    [({'arrow ajj sjj} . r)
     (let* ([ds0 ds]
            [bs0 bs]
            [gs0 gs])
       (let* ([dl1 (call-with-output-to-new-ds
                    (lambda ()
                      (push rs (% rsp-proto
                                  'ex   compose
                                  'end  rs/exit
                                  'jj   ajj))
                      (rs/next)))]
              [dl2 (pop-list ds (length dl1))])
         (if3 [(push bs '(commit-point))
               (push gs (% gsp-proto
                           'ex   cover
                           'end  bs/commit
                           'dl+  dl1
                           'dl-  dl2))
               (gs/next)]
              [sjj]
              [(set! ds ds0)
               (set! bs bs0)
               (set! gs gs0)
               (compose/try-body r)])))]))

(define (create-trunk-list t b dl)
  (let ([k (vector {'todo b dl})])
    (reverse
     (map (lambda (i) {'trunk t k i})
       (genlist (type/output-number pt))))))

(define (compose/apply j)
  (match (bs/walk (pop ds))
    [{'lambda t b}
     (compose/function t b)]
    [__ (orz 'compose/apply
          ("can not handle jo : ~a~%" j))]))

(define (compose/bind j)
  (match j
    [{'bind j vl}
     (let* ([dl (call-with-output-to-new-ds
                 (lambda ()
                   (compose/jo j)))]
            [d (car dl)])
       (if (not (eq? (length dl) 1))
         (orz 'compose/bind
           ("jo should return one data~%")
           ("but this jo does not : ~a~%" j))
         (for-each (lambda (v)
                     (bs/extend-up v d)
                     (push ds {'bind d v}))
                   vl)))]))

(define (cut)
  (let* ([rsp (pop rs)]
         [c   (^ rsp 'c)]
         [ex  (^ rsp 'ex)]
         [end (^ rsp 'end)]
         [jj  (^ rsp 'jj)])
    (if3 [(>= c (length jj))]
         [(end)]
         [(push rs (% rsp 'c (+ 1 c)))
          (cut/jo (list-ref jj c))
          (rs/next)])))

(define (cut/jo j)
  (case (car j)
    ['var           (cut/var j)]
    ['bind          (cut/bind j)]
    ['call          (cut/call j)]
    ['apply         (cut/apply j)]
    ['arrow         (cut/arrow j)]
    ['lambda        (cut/lambda j)]))

(define (cut/var j)
  ;; (if (var/fresh? j)
  ;;   (bs/extend-new j))
  (let* ([uv (v->uv j)]
         [d (bs/deep uv)])
    (let ([found-d (bs/find-up uv)])
      (if found-d
        (push ds found-d)
        (match uv
          [{'uni-var id level}
           (push ds {'uni-var id (+ 1 level)})])))))

(define (cut/call j)
  (match j
    [{'call n}
     (let ([found (assq n ns)])
       (if (not found)
         (orz 'cut/call ("unknow name : ~a~%" n))
         (match (cdr found)
           [{'meaning-type a n nl}
            (cut/type a)]
           [{'meaning-data a n n0}
            (cut/type a)]
           [{'meaning-lambda a al}
            (cut/type a)])))]))

(define (cut/type a)
  (: arrow -> !)
  (match a
    [{'arrow ajj sjj}
     (let* ([dl1 (call-with-output-to-new-ds
                  (lambda ()
                    (push rs (% rsp-proto
                                'ex   compose
                                'end  rs/exit
                                'jj   ajj))
                    (rs/next)))]
            [dl2 (pop-list ds (length dl1))])
       (if3 [(push bs '(commit-point))
             (push gs (% gsp-proto
                         'ex   unify
                         'end  bs/commit
                         'dl+  dl1
                         'dl-  dl2))
             (gs/next)]
            [(for-each compose/jo sjj)]
            [(orz 'cut/type
               ("fail on unify~%"))]))]))

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

(define (cut/bind j)
  (orz 'cut/bind
    ("can not handle bind as jo that is not in type~%")
    ("jo : ~a~%" j)))

(define print-define-flag #f)
(define (print-define+) (set! print-define-flag #t))
(define (print-define-) (set! print-define-flag #f))

(define type-check-flag #f)
(define (type-check+) (set! type-check-flag #t))
(define (type-check-) (set! type-check-flag #f))

(define-macro (def name body)
  `($def (quote ,name) (quote ,body)))

(define ($def name body)
  (let ([key (car body)])
    ((find-key key) name body)))

(define key-record '())

(define (new-key key fun)
  (set! key-record
        (cons (cons key fun)
              key-record)))

(define (find-key key)
  (let ([found (assq key key-record)])
    (if found
      (cdr found)
      (orz 'find-key
        ("can not find key : ~a~%" key)))))

(define (def-lambda n body)
  (let* ([a (compile-arrow (cadr body))]
         [al (map compile-arrow (cddr body))]
         [meaning (list 'meaning-lambda a al)])
    (push ns (cons n meaning))
    (if type-check-flag
      (type-check a al))
    (if print-define-flag
      (let ()
        (display "\n")
        (display "<def-lambda>\n")
        (display ":name: ") (display n) (display "\n")
        (display ":meaning:\n")
        (display meaning) (display "\n")
        (display "</def-lambda>\n")
        (display "\n")))))

(new-key 'lambda def-lambda)

(define (def-type n body)
  (let* ([a (compile-arrow (cadr body))]
         [pl (apply pair-list (cddr body))]
         [nl (map car pl)]
         [meaning (list 'meaning-type a n nl)])
    (push ns (cons n meaning ))
    (if print-define-flag
      (let ()
        (display "\n")
        (display "<def-type>\n")
        (display ":name: ") (display n) (display "\n")
        (display ":meaning:\n")
        (display meaning) (display "\n")
        (display "</def-type>\n")
        (display "\n")))
    (for-each (lambda (p) (def-data n p))
              pl)))

(new-key 'type def-type)

(define (def-data n0 p)
  (let* ([n (car p)]
         [a (compile-arrow (cdr p))]
         [meaning (list 'meaning-data a n n0)])
    (push ns (cons n meaning))
    (if print-define-flag
      (let ()
        (display "\n")
        (display "<def-data>\n")
        (display ":name: ") (display n) (display "\n")
        (display ":meaning:\n")
        (display meaning) (display "\n")
        (display "</def-data>\n")
        (display "\n")))))

(define (compile-arrow a)
  (pass2-arrow (pass1-arrow a)))

(define (pass1-jo jo)
  (define (var? v)
    (and (symbol? v)
         (eq? ': (symbol-car v))
         (not (eq? ': (symbol-car (symbol-cdr v))))))
  (define (fvar? v)
    (and (symbol? v)
         (eq? ': (symbol-car v))
         (eq? ': (symbol-car (symbol-cdr v)))))
  (define (bind? v)
    (and (symbol? v)
         (eq? '% (symbol-car v))
         (eq? ': (symbol-car (symbol-cdr v)))))
  (define (apply? v)
    (eq? v '@))
  (define (call? v)
    (and (symbol? v)
         (not (eq? ': (symbol-car v)))
         (not (eq? '% (symbol-car v)))))
  (define (arrow? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) '->)))
  (define (lambda? v)
    (and (list? v)
         (pair? v)
         (eq? (car v) 'lambda)))
  (cond [(var? jo)                (list 'var jo)]
        [(fvar? jo)               (list 'fvar (symbol-cdr jo))]
        [(bind? jo)               (list 'bind (symbol-cdr jo))]
        [(apply? jo)              (list 'apply)]
        [(call? jo)               (list 'call jo)]
        [(arrow? jo)              (pass1-arrow jo)]
        [(lambda? jo)             (list 'lambda (map pass1-arrow (cdr l)))]))

(define (pass1-arrow a)
  (match a
    [{'-> ac sc}
     {'arrow (map pass1-jo ac) (map pass1-jo sc)}]))

(define (pass2-jo jo)
  (match jo
    [{'arrow ac sc} (pass2-arrow jo)]
    [{'lambda al} {'lambda (map pass2-arrow al)}]
    [__ jo]))

(define (pass2-arrow a)
  (match a
    [{'arrow ac sc}
     {'arrow (jojo->var-list (append ac sc))
             (jojo->fvar-list (append ac sc))
             ac sc}]))

(define (jojo->var-list l)
  (define (one vl n)
    (if (member n vl)
      vl
      (cons n vl)))
  (define (more vl jo)
    (match jo
      [{'var n}         (one vl n)]
      [{'fvar n}        vl]
      [{'bind n}        (one vl n)]
      [{'call n}        vl]
      [{'apply}         vl]
      [{'arrow ac sc}   (loop vl (append ac sc))]
      [{'lambda al}     (arrow-loop vl al)]))
  (define (arrow-loop vl l)
    (if (null? l)
      vl
      (match (car l)
        [{'arrow ac sc}
         (arrow-loop (loop vl (append ac sc)) (cdr l))])))
  (define (loop vl l)
    (if (null? l)
      vl
      (loop (more vl (car l)) (cdr l))))
  (loop '() l))

(define (jojo->fvar-list l)
  (define (one vl n)
    (if (member n vl)
      vl
      (cons n vl)))
  (define (more vl jo)
    (match jo
      [{'var n}         vl]
      [{'fvar n}        (one vl n)]
      [{'bind n}        vl]
      [{'call n}        vl]
      [{'apply}         vl]
      [{'arrow ac sc}   (loop vl (append ac sc))]
      [{'lambda al}     (arrow-loop vl al)]))
  (define (arrow-loop vl l)
    (if (null? l)
      vl
      (match (car l)
        [{'arrow ac sc}
         (arrow-loop (loop vl (append ac sc)) (cdr l))])))
  (define (loop vl l)
    (if (null? l)
      vl
      (loop (more vl (car l)) (cdr l))))
  (loop '() l))

(define-macro (run s)
  `($run (quote ,s)))

(define ($run s)
  (for-each compose/jo (map compile/jo s)))

(define (type-check ta al)
  (: arrow {arrow ...} -> bool)
  (match ta
    [('arrow . __)
     (for-each (lambda (a) (type-check/arrow ta a))
               al)]
    [__ (orz 'type-check
          ("type of function must be arrow~%")
          ("type : ~a~%" ta))]))

(define (type-check/arrow ta a)
  (: type-arrow arrow -> bool)
  (match {ta a}
    [{{'arrow tvl tfvl tajj tsjj}
      {'arrow vl fvl ajj sjj}}
     (let* ([tvrc (vl->vrc tvl)]
            [vrc (vl->vrc vl)]
            [dl-tajj (call-with-output-to-new-ds
                      (lambda ()
                        (push rs (% rsp-proto
                                    'ex  compose
                                    'vrc tvrc
                                    'jj  tajj))
                        (rs/next)))]
            [dl-ajj (call-with-output-to-new-ds
                     (lambda ()
                       (push rs (% rsp-proto
                                   'ex  cut
                                   'vrc vrc
                                   'jj  ajj))
                       (rs/next)))]
            [dl-tsjj (call-with-output-to-new-ds
                      (lambda ()
                        (push rs (% rsp-proto
                                    'ex  compose
                                    'vrc tvrc
                                    'jj  tsjj))
                        (rs/next)))]
            [dl-sjj (call-with-output-to-new-ds
                     (lambda ()
                       (push rs (% rsp-proto
                                   'ex  cut
                                   'vrc vrc
                                   'jj  sjj))
                       (rs/next)))])
       (: ><><><
          in lack of bind-unify
          (push rs {compose <type-antecedent>})
          (push rs {compose <antecedent>})
          (push gs {bind-unify <gathered>}))
       (if3 [(push gs (% gsp-proto
                         'ex     unify
                         'dl+    dl-tajj
                         'dl-    dl-ajj))
             (gs/next)]
            [(if3 [(push gs (% gsp-proto
                               'ex     cover
                               'dl+    dl-tsjj
                               'dl-    dl-sjj))
                   (gs/next)]
                  [(: ><><><
                      in lack of undo on success)
                   #t]
                  [(orz 'type-check/arrow
                     ("cover fail~%"))])]
            [(orz 'type-check/arrow
               ("unify fail~%"))]))]))

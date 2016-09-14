(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (add : (natural natural -> natural))
  (:m zero -> :m)
  (:m :n succ -> :m :n add succ))

(define-function (mul : (natural natural -> natural))
  (:m zero -> zero)
  (:m :n succ -> :m :n mul :m add))

(define-function (factorial : (natural -> natural))
  (zero -> zero succ)
  (:n succ -> :n factorial :n succ mul))

(app (->
      zero succ
      zero succ succ
      add))

(app (->
      zero succ succ
      zero succ succ
      mul))

(app (->
      zero succ succ succ
      factorial))

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (add : (natural natural -> natural))
  (:m zero -> :m)
  (:m :n succ -> :m :n add succ))

(define-type (eq : ({:t : type} :t :t -> type))
  (refl : ({:t : type} {:d : :t} -> :d :d eq)))


(define-function (eq/test : (-> zero zero add zero zero add eq))
  (-> refl))

(define-function
    (cong : ({:t1 :t2 : type}
             {:x :y : :t1}
             :x :y eq
             (:f : (:t1 -> :t2)) ->
             :x :f :y :f eq))
  (refl (:f) -> refl))

(define-function
    (add/zero-commute : ((:m : natural) -> :m zero add zero :m add eq))
  (zero -> refl)
  (:m succ -> :m add/zero-commute (succ) cong))

;; :m zero add
;; zero :m add eq

;; :m succ zero add
;; zero :m succ add eq

;; :m zero add succ
;; zero :m add succ eq

(define-function (add/commute : ((:m :n : natural) -> :m :n add :n :m add eq))
  (:m zero -> :m add/zero-commute)
  (:m :n succ -> :m :n add/commute (succ) cong ><><><))

:m :n add/commute
;; :m :n add
;; :n :m add eq
(succ) cong
;; :m :n add succ
;; :n :m add succ eq

这里需要等式的相乘
;; :n succ :m add
;; :n :m succ add eq

><><><
;; :m :n succ add
;; :n succ :m add eq

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (drop : (:t ->))
  (:d ->))

(define-function (dup : (:t -> :t :t))
  (:d -> :d :d))

(define-function (over : (:t1 :t2 -> :t1 :t2 :t1))
  (:d1 :d2 -> :d1 :d2 :d1))

(define-function (tuck : (:t1 :t2 -> :t2 :t1 :t2))
  (:d1 :d2 -> :d2 :d1 :d2))

(define-function (swap : (:t1 :t2 -> :t2 :t1))
  (:d1 :d2 -> :d2 :d1))

(app (-> zero
         zero succ
         swap
         drop
         dup))

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (add : (natural natural -> natural))
  (:m zero -> :m)
  (:m :n succ -> :m :n add succ))

(define-function (mul : (natural natural -> natural))
  (:m zero -> zero)
  (:m :n succ -> :m :n mul :m add))

(define-type (list : (type -> type))
  (null : (-> :t list))
  (cons : (:t list :t -> :t list)))

(define-function (append : (:t list :t list -> :t list))
  (:l null -> :l)
  (:l :r :e cons -> :l :r append :e cons))

(define-function (length : (:t list -> natural))
  (null -> zero)
  (:l :e cons -> :l length succ))

(app (->
      null
      zero cons
      null
      zero cons
      append))

(app (->
      null
      zero cons
      zero cons
      null
      zero cons
      zero cons
      append
      length))

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (add : (natural natural -> natural))
  (:m zero -> :m)
  (:m :n succ -> :m :n add succ))

(define-function (mul : (natural natural -> natural))
  (:m zero -> zero)
  (:m :n succ -> :m :n mul :m add))

(define-type (list : (type -> type))
  (null : (-> :t list))
  (cons : (:t list :t -> :t list)))

(define-function (append : (:t list :t list -> :t list))
  (:l null -> :l)
  (:l :r :e cons -> :l :r append :e cons))

(define-function (map : (:t1 list (:t1 -> :t2) -> :t2 list))
  (null :f -> null)
  (:l :e cons :f -> :l :f map :e :f apply cons))

(app (->
      null
      zero cons
      zero cons
      zero cons
      null
      zero cons
      zero cons
      zero cons
      append
      (zero -> zero succ)
      map))

(app (->
      null
      zero cons
      zero cons
      (lambda (natural -> natural)
        (zero -> zero succ))
      map))

(define-type (has-length : (:t list natural -> type))
  (null/has-length : (-> null zero has-length))
  (cons/has-length : (:l :n has-length -> :l :a cons :n succ has-length)))

(define-function
    (map/has-length : (:l :n has-length -> :l :f map :n has-length))
  (null/has-length -> null/has-length)
  (:h cons/has-length -> :h map/has-length cons/has-length))

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (add : (natural natural -> natural))
  (:m zero -> :m)
  (:m :n succ -> :m :n add succ))

;; ;; this can not be used to prove append
;; (define-function (add : (natural natural -> natural))
;;    (:m zero -> :m)
;;    (zero :m -> :m)
;;    (:m succ :n succ -> :m :n add succ succ))

;; ;; this can be used to prove append
;; (define-function (add : (natural natural -> natural))
;;    (:m zero -> :m)
;;    (zero :m -> :m)
;;    (:m succ :n succ -> :m :n add succ succ)
;;    (:m :n succ -> :m :n add succ)
;;    (:m succ :n -> :m :n add succ))

(define-function (mul : (natural natural -> natural))
  (:m zero -> zero)
  (:m :n succ -> :m :n mul :m add))

(define-type (vector : (natural type -> type))
  (null : (-> zero :t vector))
  (cons : (:n :t vector :t -> :n succ :t vector)))

(define-function (append : (:m :t vector :n :t vector -> :m :n add :t vector))
  (:l null -> :l)
  (:l :r :e cons -> :l :r append :e cons))

(app (->
      null
      zero cons
      zero cons
      zero cons
      null
      zero cons
      zero cons
      zero cons
      append))

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function (add : (natural natural -> natural))
  (:m zero -> :m)
  (:m :n succ -> :m :n add succ))

(define-function (mul : (natural natural -> natural))
  (:m zero -> zero)
  (:m :n succ -> :m :n mul :m add))

(define-type (vector : (natural type -> type))
  (null : (-> zero :t vector))
  (cons : (:n :t vector :t -> :n succ :t vector)))

(define-function (append : (:m :t vector :n :t vector -> :m :n add :t vector))
  (:l null -> :l)
  (:l :r :e cons -> :l :r append :e cons))

(define-function (map : (:n :t1 vector (:t1 -> :t2) -> :n :t2 vector))
  (null (:f) -> null)
  (:l :e cons (:f) -> :l (:f) map :e :f cons))

(app (->
      null
      zero cons
      zero cons
      zero cons
      null
      zero cons
      zero cons
      zero cons
      append
      (zero -> zero succ)
      map))

(define-type (natural : type)
  (zero : natural)
  (succ : (natural -> natural)))

(define-function
    (natural-induction : ((:p : (natural -> type))
                          zero :p
                          ((:k : natural) :k :p -> :k succ :p)
                          (:x : natural) -> :x :p))
  (:q :q/z :q/s zero -> :q/z)
  (:q :q/z :q/s :n succ ->
      :n
      :q :q/z :q/s :n natural-induction
      :q/s))

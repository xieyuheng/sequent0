(define nat
  (data type
    (: zero nat)
    (: succ (-> nat nat))))

(define add
  (lambda (-> [nat nat] nat)
    (-> [:m zero] :m)
    (-> [:m :n succ] [:m :n add succ])))

(define mul
  (lambda (-> [nat nat] nat)
    (-> [:m zero] zero)
    (-> [:m :n succ] [:m :n mul :m add])))

(define factorial
  (lambda (-> nat nat)
    (-> zero [zero succ])
    (-> [:n succ] [:n factorial :n succ mul])))

(run zero succ
     zero succ succ
     add)

(run zero succ succ
     zero succ succ
     mul)

(run zero succ succ succ
     factorial)

(define nat-induction
  (lambda (-> [(: :p (-> nat type))
               zero :p @
               (-> [(: :k nat) :k :p @]
                   [:k succ :p @])
               (: :x nat)]
              [:x :p @])
    (-> [:q :q/z :q/s zero] :q/z)
    (-> [:q :q/z :q/s :n succ]
        [:n
         :q :q/z :q/s :n nat-induction
         :q/s @])))

(define drop
  (lambda (-> :t [])
    (-> :d
        [])))

(define dup
  (lambda (-> :t [:t :t])
    (-> :d
        [:d :d])))

(define over
  (lambda (-> [:t1 :t2] [:t1 :t2 :t1])
    (-> [:d1 :d2]
        [:d1 :d2 :d1])))

(define tuck
  (lambda (-> [:t1 :t2] [:t2 :t1 :t2])
    (-> [:d1 :d2]
        [:d2 :d1 :d2])))

(define swap
  (lambda (-> [:t1 :t2] [:t2 :t1])
    (-> [:d1 :d2]
        [:d2 :d1])))

(run zero
     zero succ
     swap
     drop
     dup)

(define list
  (data (-> [type] [type])
    (: null [:t list])
    (: cons (-> [:t list :t] [:t list]))))

(define append
  (lambda (-> [:t list :t list] [:t list])
    (-> [:l null] :l)
    (-> [:l :r :e cons] [:l :r append :e cons])))

(define length
  (lambda (-> [:t list] nat)
    (-> null zero)
    (-> [:l :e cons] [:l length succ])))

(run null
     zero cons
     null
     zero cons
     append)

(run null
     zero cons
     zero cons
     null
     zero cons
     zero cons
     append
     length)

(define map
  (lambda (-> [:t1 list (-> :t1 :t2)]
              [:t2 list])
    (-> [null :f] null)
    (-> [:l :e cons :f] [:l :f map :e :f @ cons])))

(run null
     zero cons
     zero cons
     zero cons
     null
     zero cons
     zero cons
     zero cons
     append
     (-> zero [zero succ])
     map)

(run null
     zero cons
     zero cons
     (lambda (-> nat nat)
       (-> zero [zero succ]))
     map)

(define has-length
  (data (-> [:t list nat] type)
    (: null/has-length [null zero has-length])
    (: cons/has-length (-> [:l :n has-length]
                           [:l :a cons :n succ has-length]))))

(define map/has-length
  (lambda (-> [:l :n has-length]
              [:l :f map :n has-length])
    (-> null/has-length null/has-length)
    (-> [:h cons/has-length] [:h map/has-length cons/has-length])))

(define vector
  (data (-> [nat type] type)
    (: null [zero :t vector])
    (: cons (-> [:n :t vector :t]
                [:n succ :t vector]))))

(define append
  (lambda (-> [:m :t vector :n :t vector]
              [:m :n add :t vector])
    (-> [:l null] :l)
    (-> [:l :r :e cons]
        [:l :r append :e cons])))

(run null
     zero cons
     zero cons
     zero cons
     null
     zero cons
     zero cons
     zero cons
     append)


(define map
  (lambda (-> [:n :t1 vector (-> :t1 :t2)]
              [:n :t2 vector])
    (-> [null :f] null)
    (-> [:l :e cons :f] [:l :f map :e :f @ cons])))

(run null
     zero cons
     zero cons
     zero cons
     null
     zero cons
     zero cons
     zero cons
     append
     (-> zero [zero succ])
     map)

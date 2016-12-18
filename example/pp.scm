:counter: 0
:explainer: (unify 'cover)

( <trunk>
  #0 :todo:
  :dl:
  ( <var>
    :f #737 ^0
    :data: ( -> [ :t1 ] [ :t2 ] ) )
  ( <var>
    :l #736 ^0
    :data: [ list ( <var>
                    :t #739 ^0
                    :type: ( <var>
                             :t1 #741 ^0
                             :type: ( <var>
                                      :t1 #749 ^0
                                      :data: type )
                             :data: type )
                    :data: type ) ] )
  :ual:
  ( -> [ null :f ]
       [ null ] )
  ( -> [ :l :e cons :f ]
       [ :l :f map :e :f @ cons ] ) )

( <trunk>
  #0 :todo:
  :dl:
  ( <var>
    :f #678 ^0
    :data: ( -> [ :t1 ] [ :t2 ] ) )
  ( <var>
    :l #686 ^0
    :type: ( <var>
             :l #727 ^0
             :type: ( <var>
                      :l #736 ^0
                      :data: [ list ( <var>
                                      :t #739 ^0
                                      :type: ( <var>
                                               :t1 #741 ^0
                                               :type: ( <var>
                                                        :t1 #749 ^0
                                                        :data: type )
                                               :data: type )
                                      :data: type ) ] )
             :data: [ list ( <var>
                             :t #730 ^0
                             :data: type ) ] )
    :data: [ list ( <var>
                    :t #689 ^0
                    :type: ( <var>
                             :t #690 ^0
                             :type: ( <var>
                                      :t #692 ^0
                                      :type: ( <var>
                                               :t #691 ^0
                                               :type: ( <var>
                                                        :t #695 ^0
                                                        :type: ( <var>
                                                                 :t1 #694 ^0
                                                                 :type: ( <var>
                                                                          :t1 #703 ^0
                                                                          :type: ( <var>
                                                                                   :t1 #715 ^0
                                                                                   :type: ( <var> :t1 #723 ^0 )
                                                                                   :data: type )
                                                                          :data: type )
                                                                 :data: type )
                                                        :data: type )
                                               :data: type )
                                      :data: type )
                             :data: type )
                    :data: type ) ] )
  :ual:
  ( -> [ null :f ]
       [ null ] )
  ( -> [ :l :e cons :f ]
       [ :l :f map :e :f @ cons ] ) )

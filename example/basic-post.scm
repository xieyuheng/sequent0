[ <>   [ ] [ type ] ->
  zero [ ] [ <> ] ->
  succ [ <> ] [ <> ] ->
] type
[ def ] nat

[ [ nat nat ] [ nat ] ->
  [ :m zero ] [ :m ] ->
  [ :m :n succ ] [ :m :n add succ ] ->
] lambda
[ add ] def


[ <>   [ ] [ type ] ->
  zero [ ] [ <> ] ->
  succ [ <> ] [ <> ] ->
] type
[ def ] nat

[ [ nat nat ]
  [ nat ] ->
  [ :m zero ]
  [ :m ] ->
  [ :m :n succ ]
  [ :m :n add succ ] ->
] lambda
[ add ] def

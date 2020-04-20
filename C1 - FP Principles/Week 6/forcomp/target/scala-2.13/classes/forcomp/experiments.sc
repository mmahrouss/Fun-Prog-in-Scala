import forcomp.Anagrams._
val abba = List(('a', 2), ('b', 2))
val abbaa = List(('a', 3), ('b', 2))
val abbbaaa = List(('a', 4), ('b', 3))
val abbacomb = List(
  List(),
  List(('a', 1)),
  List(('a', 2)),
  List(('b', 1)),
  List(('a', 1), ('b', 1)),
  List(('a', 2), ('b', 1)),
  List(('b', 2)),
  List(('a', 1), ('b', 2)),
  List(('a', 2), ('b', 2))
)
val r = combinations(abba)
r.toSet
r.length
r.toSet.size
abbacomb.toSet
subtract(abbaa, abba)
subtract(abbbaaa, abba)
//List(List()).flatten

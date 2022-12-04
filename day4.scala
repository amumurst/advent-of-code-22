def parseFile = readFile("4")
  .collect { case s"${Int(a)}-${Int(b)},${Int(c)}-${Int(d)}" => (a to b, c to d) }
@main def day4a =
  parseFile.count((ab, cd) => ab.containsSlice(cd) || cd.containsSlice(ab)).calculate

@main def day4b =
  parseFile.count((ab, cd) => ab.intersect(cd).nonEmpty).calculate

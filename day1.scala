private def solve(elements: List[String], m: List[Int], c: Int): Int =
  elements match
    case "" :: rest     => solve(rest, (m :+ c).sorted.tail, 0)
    case Int(i) :: rest => solve(rest, m, c + i)
    case err :: rest    => s"Bad line $err".print; solve(rest, m, c)
    case Nil            => m.sum

@main def day1a =
  solve(readFile("1"), List(0), 0).calculate

@main def day1b =
  solve(readFile("1"), List(0, 0, 0), 0).calculate

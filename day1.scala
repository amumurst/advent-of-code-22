private def solve(elements: List[String], maxes: List[Int], current: Int): Int =
  elements match
    case "" :: rest     => solve(rest, (maxes :+ current).sorted.tail, 0)
    case Int(i) :: rest => solve(rest, maxes, current + i)
    case err :: rest    => println(s"Bad line $err"); solve(rest, maxes, current)
    case Nil            => maxes.sum

@main def day1a =
  solve(readFile("1"), List(0), 0).calculate

@main def day1b =
  solve(readFile("1"), List(0, 0, 0), 0).calculate

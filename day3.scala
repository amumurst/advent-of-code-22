extension (ss: List[String])
  private def valueCommonLettersOfSacks(f: List[String] => List[List[String]]): List[Int] =
    def valueLetter(letter: Char): Int = if (letter.isLower) letter - 'a' + 1 else letter - 'A' + 27
    def singleCommonLetter(s: List[String]): Option[Char] =
      s.map(_.toSet) match
        case head :: rest => rest.foldLeft(head)(_ intersect _).headOption
        case _            => None
    f(ss).flatMap(singleCommonLetter).map(valueLetter)

@main def day3a =
  readFile("3")
    .valueCommonLettersOfSacks(sack => sack.splitAt(sack.size / 2).toList)
    .sum
    .calculate

@main def day3b =
  readFile("3")
    .valueCommonLettersOfSacks(_.grouped(3).toList)
    .sum
    .calculate

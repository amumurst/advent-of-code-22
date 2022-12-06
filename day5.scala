extension (lines: List[String])
  private def recurse(columns: List[List[Char]])(using shouldReverse: Boolean): String = lines match
    case s"move ${Int(n)} from ${Int(from)} to ${Int(to)}" :: rest =>
      val newColumns = columns.zipWithIndex.map {
        case (s, i) if i == to - 1 =>
          val movedBoxes = columns(from - 1).take(n)
          if (shouldReverse) movedBoxes.reverse :++ s else movedBoxes :++ s
        case (s, i) if i == from - 1 => s.drop(n)
        case (s, _)                  => s
      }

      rest.recurse(newColumns)
    case "" :: rest =>
      //Transpose the rows to columns and pick out only the letters
      val allBoxes = columns.transpose.collect { case s if s.exists(_.isDigit) => s.filter(_.isLetter) }
      rest.recurse(allBoxes)
    case box :: rest => rest.recurse(columns :+ box.toList)
    case Nil         => columns.flatMap(_.headOption).mkString

@main def day5a = readFile("5").recurse(Nil)(using true).calculate
@main def day5b = readFile("5").recurse(Nil)(using false).calculate

extension (strings: List[String])
  private def parse(files: List[(List[String], Int)], dir: List[String]): List[(List[String], Int)] =
    strings match
      case s"$_ cd /" :: rest            => rest.parse(files, Nil)
      case s"$_ cd .." :: rest           => rest.parse(files, dir.dropRight(1))
      case s"$_ cd $target" :: rest      => rest.parse(files, dir :+ target)
      case (s"$_ ls" | s"dir$_") :: rest => rest.parse(files, dir)
      case s"$size $_" :: rest           => rest.parse(files :++ dir.reverse.tails.map(_.reverse -> size.toInt), dir)
      case _                             => files

@main def day7a =
  readFile("7")
    .parse(Nil, Nil)
    .groupMapReduce(_._1)(_._2)(_ + _)
    .collect { case (_, size) if size < 100_000 => size }
    .sum
    .calculate

@main def day7b =
  readFile("7")
    .parse(Nil, Nil)
    .groupMapReduce(_._1)(_._2)(_ + _)
    .pipe { dirSizes =>
      val neededSpace = dirSizes(Nil) - 70_000_000 + 30_000_000
      dirSizes.collect { case (_, size) if size > neededSpace => size }.min
    }
    .calculate

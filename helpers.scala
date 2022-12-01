export scala.util.chaining._
def readFile(task: String) = scala.io.Source.fromFile(s"input/$task.txt").getLines().toList

object Int:
  def unapply(s: String): Option[Int] = s.toIntOption

extension [A](a: => A)
  def print: Unit = println(a)

  def calculate: Unit =
    val start = System.currentTimeMillis()
    val _a    = a
    val end   = System.currentTimeMillis()
    s"Task took ${end - start}ms".print
    _a.print

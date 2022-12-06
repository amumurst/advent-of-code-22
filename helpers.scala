export scala.util.chaining._
def readFile(task: String) = scala.io.Source.fromFile(s"input/$task.txt").getLines().toList
def readFileLine(task: String) = readFile(task).mkString

object Int:
  def unapply(s: String): Option[Int] = s.toIntOption

extension [A](a: => A)
  def calculate: Unit =
    val start = System.currentTimeMillis()
    val _a    = a
    val end   = System.currentTimeMillis()
    println(s"Task took ${end - start}ms")
    println(s"Result was $_a")

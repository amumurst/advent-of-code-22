case class Computer[A](x: Int, cycle: Int, state: A, recalcState: Computer[A] => A):
  def tick(addX: Int) = Computer[A](x + addX, cycle + 1, recalcState(this), recalcState)
  def runProgram(s: List[String]): A =
    s match
      case "noop" :: rest            => tick(0).runProgram(rest)
      case s"addx ${Int(i)}" :: rest => tick(0).runProgram(s"x$i" +: rest)
      case s"x${Int(i)}" :: rest     => tick(i).runProgram(rest)
      case _                         => recalcState(this)
def emptyComputer[A](state: A, recalcState: Computer[A] => A) = Computer(1, 1, state, recalcState)

@main def day10a =
  def sumSignal(s: Computer[Int]): Int = if ((s.cycle - 20) % 40 == 0) s.state + s.x * s.cycle else s.state
  readFile("10").pipe(emptyComputer(0, sumSignal).runProgram).calculate

@main def day10b =
  def printSprites(s: Computer[Unit]): Unit =
    val carretPos = (s.cycle - 1) % 40
    if (carretPos == 0) print("\n")
    if (Set(-1, 0, 1).contains(carretPos - s.x)) print('#') else print('.')
  readFile("10").pipe(emptyComputer((), printSprites).runProgram).calculate

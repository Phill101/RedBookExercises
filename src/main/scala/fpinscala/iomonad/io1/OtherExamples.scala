package fpinscala.iomonad.io1

object OtherExamples extends App {
  val echo = IO1.ReadLine.flatMap(IO1.PrintLine)
  val readInt = IO1.ReadLine.map(_.toInt).map(_ + 1).map(_.toString).flatMap(IO1.PrintLine)
  val readInts = IO1.IO.map2(IO1.ReadLine, IO1.ReadLine)((a, b) => (a.toInt + b.toInt).toString).flatMap(IO1.PrintLine)

  echo.run
  readInt.run
  readInts.run

  IO1.factorialREPL.run
}

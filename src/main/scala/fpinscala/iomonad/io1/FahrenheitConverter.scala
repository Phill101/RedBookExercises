package fpinscala.iomonad.io1

import fpinscala.iomonad.io0.IO0

object FahrenheitConverter extends App {

  def converter: IO1.IO[Unit] = for {
    _ <- IO1.PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- IO1.ReadLine.map(_.toDouble)
    _ <- IO1.PrintLine(IO0.fahrenheitToCelsius(d).toString)
  } yield ()


  converter.run
}

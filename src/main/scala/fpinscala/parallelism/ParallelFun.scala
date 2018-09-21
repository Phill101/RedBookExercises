package fpinscala.parallelism

import fpinscala.parallelism.Nonblocking.Par._
import java.util.concurrent.Executors

object ParallelFun extends App {
  val es = Executors.newFixedThreadPool(2)


}

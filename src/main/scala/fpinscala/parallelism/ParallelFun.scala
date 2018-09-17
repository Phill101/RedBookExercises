package fpinscala.parallelism

import java.util.concurrent.TimeUnit
import fpinscala.parallelism.Par._
import java.util.concurrent.Executors

object ParallelFun extends App {
  val es = Executors.newFixedThreadPool(100)

  val units = List(
    unit(1), unit(2), unit(3), unit(4)
  )
  val unitSeq = Par.sequence(units)(es)
  println(s"unitSeq=$unitSeq")


  val array = scala.util.Random.shuffle((1 to 10).toBuffer).toIndexedSeq

  val sum     = Par.parSum(array)(es)
  val product = Par.parProduct(array)(es)
  val max     = Par.parMax(array)(es)
  val min     = Par.parMin(array)(es)

  val sumR = sum.get(5, TimeUnit.SECONDS)
  val prdR = product.get(5, TimeUnit.SECONDS)
  val maxR = max.get(5, TimeUnit.SECONDS)
  val minR = min.get(5, TimeUnit.SECONDS)

  println(
    s"""
       |sumR=$sumR
       |prdR=$prdR
       |maxR=$maxR
       |minR=$minR
     """.stripMargin)

  es.shutdown()
  if (es.awaitTermination(5, TimeUnit.SECONDS)) es.shutdownNow()
}

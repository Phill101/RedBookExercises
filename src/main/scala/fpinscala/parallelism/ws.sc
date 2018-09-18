import fpinscala.parallelism.Par._
import java.util.concurrent.Executors

import fpinscala.parallelism.Par

val es = Executors.newFixedThreadPool(100)


Par.map3(unit(1), unit(2), unit(3))(_.toString + _.toString + _.toString)(es)
Par.map4(unit(1), unit(2), unit(3), unit(4))(_.toString + _.toString + _.toString + _.toString)(es)
Par.map5(unit(1), unit(2), unit(3), unit(4), unit(5))(_.toString + _.toString + _.toString + _.toString + _.toString)(es)
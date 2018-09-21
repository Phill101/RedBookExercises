import fpinscala.parallelism.Par._
import java.util.concurrent.Executors

import fpinscala.parallelism.Par

val es = Executors.newFixedThreadPool(100)


choice(unit(true))(unit(1), unit(2))(es)
package fpinscala.monoids

object TreeTest extends App {
  val tree = Branch(Leaf(1), Leaf(2))
  val flRes = TreeFoldable.foldLeft(tree)("!")(_ + _.toString)
  val frRes = TreeFoldable.foldRight(tree)("!")(_.toString + _)
  println(flRes)
  println(frRes)
}

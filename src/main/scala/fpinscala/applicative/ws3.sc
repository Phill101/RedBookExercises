import fpinscala.applicative.{Applicative, Traverse, Tree}

import scala.language.higherKinds

val optApplicative = new Applicative[Option] {
  override def unit[A](a: => A) = Some(a)

  override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
}


optApplicative.map2(Option(1), Option.empty[Int])(_ + _)

optApplicative.sequence(List(Some(1), Some(2)))

optApplicative.sequenceMap(
  Map(
    "1" -> Some(1),
    "2" -> Some(2),
    "3" -> Some(3)
  )
)

val listApplicative = new Applicative[List] {
  override def unit[A](a: => A) = List(a)
  override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C) =
    fa.zip(fb).map(f.tupled)
}

Traverse.listTraverse.sequence(List(Option(1), Option(2)))(optApplicative)
Traverse.optionTraverse.sequence(Option(List(1)))(listApplicative)


Traverse.listTraverse.reverse(List(1, 2, 3))
Traverse.optionTraverse.reverse(Option(1))

val tree = Tree(1,
  List(
    Tree(2, Nil),
    Tree(3, Nil),
    Tree(4, Nil)
  ))
Traverse.treeTraverse.reverse(tree)

Traverse.listTraverse.foldLeft(List(1, 2, 3, 4))(-100)(_ + _)
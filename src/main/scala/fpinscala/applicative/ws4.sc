import fpinscala.applicative.{Applicative, Monad, Traverse}

val optTraverse = Traverse.optionTraverse
val lstTraverse = Traverse.listTraverse

implicit val optApplicative = new Applicative[Option] {
  override def unit[A](a: => A) = Some(a)

  override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
}

implicit val listApplicative: Applicative[List] = new Applicative[List] {
  override def unit[A](a: => A) = List(a)
  override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C) =
    fa.zip(fb).map(f.tupled)
}

case class Box[T](v: T)
val boxApplicative: Applicative[Box] = new Applicative[Box] {
  override def unit[A](a: => A): Box[A] = Box(a)
  override def map2[A, B, C](fa: Box[A], fb: Box[B])(f: (A, B) => C) =
    Box(f(fa.v, fb.v))
}

optTraverse.fuse(Option(1))(a => List(a, a), a => List(a, a))


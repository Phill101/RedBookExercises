import fpinscala.applicative.Applicative

val listApplicative = new Applicative[List] {
  override def unit[A](a: => A) = List(a)
  override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
    fa.zip(fb).map(f.tupled)
}

val optionApplicative = new Applicative[Option] {
  override def unit[A](a: => A) = Some(a)
  override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
}

val optionListComposition = optionApplicative.compose(listApplicative)


optionListComposition.unit(1)

val listOptionProduct = listApplicative.product(optionApplicative)
val q = listOptionProduct.unit(1)
val w = listOptionProduct.unit(2)

listOptionProduct.map2(q, w) { (a, b) =>
  a + b
}

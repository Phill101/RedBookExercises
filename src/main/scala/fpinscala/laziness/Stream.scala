package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n-1))
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def head: A = this match {
    case Cons(h, _) => h()
    case _ => throw new Exception("Head on empty stream")
  }

  def tail: Stream[A] = this match {
    case Cons(_, t) => t()
    case _ => throw new Exception("Tail on empty stream")
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) =>
    if (p(a)) Stream.cons(a, b) else b
  }

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def append[B >: A](a: => Stream[B]): Stream[B] = foldRight(a)((h, t) => Stream.cons(h, t))

  def forall(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, _) => Stream.cons[B](f(a), tail.map(f)))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) =>  if (f(a)) Stream.cons(a, b) else b)

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forall {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

  /**
    * The zipAll function should continue the traversal as long as either stream
    * has more elements—it uses Option to indicate whether each stream has been
    * exhausted.
    */
  def zipAll[B](s2: Stream[B]): Stream[ (Option[A], Option[B]) ] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some(s, s drop 1)
  }

  def tailsViaFoldRight: Stream[Stream[A]] = scanRight(Stream.empty[A])(Stream.cons(_, _))

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  /*
    The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
    It can be implemented using `foldRight` though. The implementation is just a `foldRight` that keeps the accumulated value
    and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds,
    it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

//  val ones: Stream[Int] = Stream.cons(1, ones)
  val ones: Stream[Int] = constant(1)

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

//  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def from(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

//  def fibs(first: Int, second: Int): Stream[Int] = cons(first, fibs(second, second + first))
  def fibs(first: Int, second: Int): Stream[Int] = unfold((first, second)) {
    case (x, y) => Some(x, (y, x+y))
  }

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take[A](s: Stream[A])(n: Int): Stream[A] = unfold((n, s)) {
    case (c, Cons(h, t)) if c > 0 => Some(h(), (c-1, t()))
    case _ => None
  }

  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = unfold(s) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

}
package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {                                   // map2( map2(a, b)(f.curried(_)(_)), c )(_(_))
    val part = map2(a, b)((a, b) => f.curried(a)(b))
    map2(part, c)(_.apply(_))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {                  // map2( map3(a, b, c)(f.curried(_)(_)(_)), d )(_(_))
    val part = map3(a, b, c)((a, b, c) => f.curried(a)(b)(c))
    map2(part, d)(_.apply(_))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = { // map2( map4(a, b, c, d)(f.curried(_)(_)(_)(_)), e )(_(_))
    val part = map4(a, b, c, d)((a, b, c, d) => f.curried(a)(b)(c)(d))
    map2(part, e)(_.apply(_))
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft(unit(List.empty[A])) { (acc, v) =>
      acc.map2(v)((l, a) => a :: l)
    }.map(_.reverse)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fas = as.map(asyncF {
      a => Some(a).filter(f)
    })
    sequence(fas).map(_.flatten)
  }

  def parFold[A](as: IndexedSeq[A])(z: A)(f: (A, A) => A): Par[A] = fork {
    if (as.size <= 1)
      Par.unit(as.headOption getOrElse z)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(parFold(l)(z)(f), parFold(r)(z)(f))(f)
    }
  }

  def countParagraphs(l: List[String]): Par[Int] = {
    val paragraphsLengths = l.map(s => lazyUnit(s.split(" ").length))
    sequence(paragraphsLengths).map(_.sum)
  }

  def parSum(as: IndexedSeq[Int]): Par[Int] = parFold(as)(0)(_ + _)
  def parProduct(as: IndexedSeq[Int]): Par[Int] = parFold(as)(0)(_ * _)
  def parMax(as: IndexedSeq[Int]): Par[Int] = parFold(as)(as.head)(_ max _)
  def parMin(as: IndexedSeq[Int]): Par[Int] = parFold(as)(as.head)(_ min _)

  /* Gives us infix syntax for `Par`. */
  implicit class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p, b)(f)
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def head[A](l: List[A]): A = l match {
    case Nil => throw new UnsupportedOperationException("head of empty list")
    case Cons(h, _) => h
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    if (f(head(l))) dropWhile(tail(l), f)
    else l

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec def go(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }
    go(l, z)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  // it's not stack safe
  // we should use reverse
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, identity[B] _){ (a, b) => bv => b(f(bv, a)) }(z)
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, identity[B] _){ (b, a) => bv => b(f(a, bv)) }(z)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldLeft(reverse(l), Nil: List[B])((b, a) => Cons(f(a), b))

  def flatten[A](as: List[List[A]]): List[A] = flattenViaFoldLeft(as)

  def flattenViaFoldLeft[A](as: List[List[A]]): List[A] =
    foldLeft(reverse(as), Nil: List[A])((b, a) => append(a, b))

  def flattenViaFoldRight[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(reverse(as), Nil: List[A])((b, a) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(i => if (f(i)) List(i) else Nil)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(supt: List[A], subt: List[A], res: Boolean): Boolean = {
      (supt, subt) match {
        case (l @ Cons(lh, lt), r @ Cons(rh, rt)) =>
          if (lh == rh) go(lt, rt, res = true)
          else if (r eq sub) go(lt, sub, res = false)
          else go(l, sub, res = false)
        case (Nil, _) | (_, Nil) => res
      }
    }
    go(sup, sub, res = false)
  }
}

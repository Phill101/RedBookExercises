package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[Any]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int =
    fold(t)(n => n)(_ max _)

  def depth(t: Tree[Any]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n) => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}
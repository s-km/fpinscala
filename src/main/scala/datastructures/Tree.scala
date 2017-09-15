package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
  def size2[A](t: Tree[A]): Int = fold(t)((_: A) => 1)((_ + _ + 1))

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  def maximum2(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depth[A](t: Tree[A], pathLength: Int = 0): Int = t match {
    case Leaf(_) => pathLength
    case Branch(l, r) => depth(l, pathLength + 1) max depth(r, pathLength + 1)
  }
  def depth2[A](t: Tree[A]): Int =
    fold(t)((_: A) => 0)((x, y) => (x + 1) max (y + 1))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

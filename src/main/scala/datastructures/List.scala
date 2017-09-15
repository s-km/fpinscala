package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) =>
        x + sum(xs)
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Empty list does not have a tail")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Empty list does not have a head to replace")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)((a2s, x) => Cons(x, a2s))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def naiveFoldLeftRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))
  def foldLeftRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (in: B) => in)((a, g) => acc => g(f(acc, a)))(z)

  def foldRightLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (in: B) => in)((g, a) => acc => g(f(a, acc)))(z)

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def doublesToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doublesToString(xs))
  }

  def addOne2(l: List[Int]): List[Int] = map(l)(_ + 1)
  def doublesToString2(l: List[Double]): List[String] =
    map(l)((d: Double) => d.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(a, as) if (f(a)) => Cons(a, filter(as)(f))
    case Cons(_, as) => filter(as)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    def go[A, B](as: List[A])(f: A => List[B]): List[List[B]] = as match {
      case Nil => Nil
      case Cons(a, as) => Cons(f(a), go(as)(f))
    }
    concat(go(as)(f))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def intZip(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, intZip(xs, ys))
  }

  def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] =
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x, xs) =>
      length(sup) >= length(sub) &&
        (foldLeft(zipWith(sup, sub)(_ == _), true)(_ && _) || hasSubsequence(xs, sub))
  }
}

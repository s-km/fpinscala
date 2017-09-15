package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the
  // function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) =>
      f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
    case _ => z
  }

  // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(Nil: List[A])(_ :: _)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _ => Empty
  }
  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if (n > 0) => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }
  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)
  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => f(a).append(b))

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h, t), Cons(hB, tB)) => Some((f(h(), hB()), (t(), tB())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (e @ Empty, Cons(h, t)) => Some((None, Some(h())), (e, t()))
      case (Cons(h, t), e @ Empty) => Some((Some(h()), None), (t(), e))
      case (Cons(h, t), Cons(hB, tB)) =>
        Some((Some(h()), Some(hB())), (t(), tB()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]) =
    zipAll(s)
      .takeWhile { case (t, s) => !s.isEmpty }
      .forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _ => None
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //Does the same as scanRight but is fully lazy - runs much faster
  //at the expense of being significantly slower to actually process
  //(e.g. tolist) the entire stream
  def lazyScanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.map(s => s.foldRight(z)(f))
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val c = b
      (f(a, c._1), cons(f(a, c._1), c._2))
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

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(prev: Int, cur: Int): Stream[Int] =
      Stream.cons(prev, go(cur, prev + cur))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }
}

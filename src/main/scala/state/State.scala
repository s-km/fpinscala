package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

// NB - this was called SimpleRNG in the book text
object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      val (b, rng3) = g(a)(rng2)
      (b, rng3)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def fmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  def fmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, ns) = rng.nextInt
    (if (i < 0) -(i + 1) else i, ns)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, ns) = nonNegativeInt(rng)
    (i.toDouble - 1 / Int.MaxValue, ns)
  }
  val elegantDouble: Rand[Double] =
    map(nonNegativeInt)(_.toDouble - 1 / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, ns) = nonNegativeInt(rng)
    val (d, ns2) = double(ns)
    ((i, d), ns2)
  }
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), ns) = intDouble(rng)
    ((d, i), ns)
  }
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, ns) = double(rng)
    val (d2, ns2) = double(ns)
    val (d3, ns3) = double(ns2)
    ((d1, d2, d3), ns3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (Nil, rng)
    else {
      val (i, ns) = int(rng)
      val (is, ns2) = ints(count - 1)(ns)
      (i :: is, ns2)
    }
  def sequencedInts(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  //type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit(Nil): State[S, List[A]])((a, b) => a.map2(b)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def go(input: Input): Machine => Machine =
      (m: Machine) =>
        input match {
          case Coin if (m.locked && m.candies > 0) => Machine(!m.locked, m.candies, m.coins + 1)
          case Turn if (!m.locked && m.candies > 0) => Machine(!m.locked, m.candies - 1, m.coins)
          case _ => m
      }

    for {
      _ <- sequence(inputs.map(i => modify(go(i))))
      m <- get
    } yield (m.coins, m.candies)
  }

}

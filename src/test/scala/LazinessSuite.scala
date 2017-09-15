package fpinscala.laziness

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._

import Stream._

class LazinessSuite extends FlatSpec with GeneratorDrivenPropertyChecks {
  import StreamGenerator._

  "Stream" should "implement take, takeWhile, and map in terms of unfold correctly" in {
    forAll { (ais: Stream[Int], n: Int, f: Int => String, p: Int => Boolean) => 
      ais.takeUnfold(n) === ais.take(n)
      ais.takeWhileUnfold(p) === ais.takeWhile(p)
      ais.mapUnfold(f) === ais.map(f) 
    }
  }
  it should "implement takeWhile in terms of foldRight correctly" in {
    forAll { (ais: Stream[Int], p: Int => Boolean) =>
      ais.takeWhileFR(p) === ais.takeWhile(p)
    }
  }
  it should "correctly identify subsequences in a `List`" in {
    val l = Stream(1, 2, 3, 4, 5)

    assert(l.hasSubsequence(Empty) === true)
    assert(l.hasSubsequence(l) === true)
    assert(l.hasSubsequence(Stream(1)) === true)
    assert(l.hasSubsequence(Stream(5)) === true)
    assert(l.hasSubsequence(Stream(10)) === false)
    assert(l.hasSubsequence(Stream(1, 2, 3)) === true)
    assert(l.hasSubsequence(Stream(3, 4)) === true)
    assert(l.hasSubsequence(Stream(1, 3, 2)) === false)
    assert(Empty.hasSubsequence(Empty) === true)
  } //TODO: Test more stuff

}

object StreamGenerator {
  implicit def arbStream[A](implicit A: Arbitrary[A]): Arbitrary[Stream[A]] = {
    lazy val stream: Gen[Stream[A]] = Gen.lzy(Gen.sized { size =>
      if (size <= 0)
          for {
            v <- A.arbitrary
            select = scala.util.Random.nextInt
          } yield if(select % 13 == 0) Empty else Stream(v)
        else
          for {
            n <- Gen.choose(1, size)
            l <- A.arbitrary
            r <- Gen.resize(size - n, stream)
          } yield cons(l, r)
      })
    Arbitrary(stream)
  }
}

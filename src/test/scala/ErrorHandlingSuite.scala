package fpinscala.errorhandling

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._
import scala.util.Random.nextInt

import Either._
import Option._

class ErrorHandlingSuite extends FlatSpec with GeneratorDrivenPropertyChecks {
  import DataStructureGenerator._
  
  "Either" should "correctly sequence a `List` of `Either`s" in {
    forAll { (j: Int) => 
      val noLefts = List.fill(nextInt(100))(Right(j))
      val unwrapped = List.fill(nextInt(100))(j)
      Either.sequence(noLefts) === Right(unwrapped)
    }
  }
  it should "correctly traverse a `List`" in {
    forAll { (ail: List[Int]) => 
      val asl = ail.map(_.toString)
      val f = (i: Int) => Either.Try(i.toString)
      Either.traverse(ail)(f) === Right(asl)
    }
  } //TODO: Test more stuff

  "Option" should "implement flatMap, orElse, and filter in terms of map correctly" in {
    forAll { (aoi: Option[Int], ob: Option[String], f: Int => Option[Int], g: Int => Boolean) =>
      aoi.flatMap(f) === aoi.flatMap2(f)
      aoi.orElse(ob) === aoi.orElse2(ob)
      aoi.filter(g) === aoi.filter2(g)
    }
  } //TODO: Test more stuff

}

object DataStructureGenerator {
  implicit def arbOption[A](implicit A: Arbitrary[A]): Arbitrary[Option[A]] = {
    lazy val option: Gen[Option[A]] = Gen.lzy(Gen.sized { size =>
      if (size % 3 == 0)
        for (e <- A.arbitrary) yield None
      else
        for(a <- A.arbitrary) yield Some(a)
    })
  Arbitrary(option)
  }

  implicit def arbEither[E, A](implicit
    A: Arbitrary[A],
    E: Arbitrary[E]): Arbitrary[Either[E, A]] = {
    lazy val either: Gen[Either[E, A]] = Gen.lzy(Gen.sized { size =>
      if (size % 3 == 0)
        for (e <- E.arbitrary) yield Left(e)
      else
        for (a <- A.arbitrary) yield Right(a)
    })
  Arbitrary(either)
  }
}

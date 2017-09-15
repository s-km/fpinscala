package fpinscala.datastructures

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._

import Tree._
import List._

class DataStructureSuite extends FlatSpec with GeneratorDrivenPropertyChecks {
  import DataStructureGenerator._
  
  "List" should "implement foldLeft in terms of foldRight and vice versa correctly" in {
    forAll { (ail: List[Int]) =>
      foldLeft(ail, 0)(_ + _) === foldLeftRight(ail, 0)(_ + _)
      foldRight(ail, 0)(_ + _) === foldRightLeft(ail, 0)(_ + _)
    }
  }
  it should "correctly identify subsequences in a `List`" in {
    val l = List(1, 2, 3, 4, 5)

    assert(hasSubsequence(l, Nil) === true)
    assert(hasSubsequence(l, l) === true)
    assert(hasSubsequence(l, List(1)) === true)
    assert(hasSubsequence(l, List(5)) === true)
    assert(hasSubsequence(l, List(10)) === false)
    assert(hasSubsequence(l, List(1, 2, 3)) === true)
    assert(hasSubsequence(l, List(3, 4)) === true)
    assert(hasSubsequence(l, List(1, 3, 2)) === false)
    assert(hasSubsequence(Nil, Nil)=== false)
  }

  "Tree" should "implement map, depth, maximum and size in terms of fold correctly" in {
    forAll { (ti: Tree[Int], f: Int => Int) => 
      size(ti) === size2(ti)
      depth(ti) === depth2(ti)
      maximum(ti) === maximum2(ti)
      Tree.map(ti)(f) === map2(ti)(f)
    }
  } //TODO: Test more stuff

}

object DataStructureGenerator {
  implicit def arbList[A](implicit A: Arbitrary[A]): Arbitrary[List[A]] = {
    lazy val list: Gen[List[A]] = Gen.lzy(Gen.sized { size =>
      if (size <= 0) for {
        v <- A.arbitrary
        select = scala.util.Random.nextInt
      } yield if(select % 13 == 0) Nil else List(v)
      else for {
        n <- Gen.choose(1, size)
        l <- A.arbitrary
        r <- Gen.resize(size - n, list)
      } yield Cons(l, r)
    })
    Arbitrary(list)
  }

  implicit def arbTree[A](implicit A: Arbitrary[A]): Arbitrary[Tree[A]] = {
    lazy val tree: Gen[Tree[A]] = Gen.lzy(Gen.sized { size =>
      if (size <= 0) for {
        v <- A.arbitrary
      } yield Leaf(v) else for {
        n <- Gen.choose(1, size)
        l <- Gen.resize(n - 1, tree)
        r <- Gen.resize(size - n, tree)
      } yield Branch(l, r)
    })
    Arbitrary(tree)
  }
}
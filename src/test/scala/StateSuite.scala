package fpinscala.state

import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._

import State._
import RNG._

class StateSuite extends FlatSpec with GeneratorDrivenPropertyChecks {
	import StateGenerator._

	val turn = List(Turn)
	val coin = List(Coin)

	"RNG" should "implement map and map2 in terms of flatMap correctly" in {
		forAll { (r1: Rand[Int], r2: Rand[String], f: Int => String, g: (Int, String) => String) => 
			map(r1)(f) === fmap(r1)(f)
			map2(r1, r2)(g) === fmap2(r1, r2)(g)
		}
	}
	it should "always generate the same pseudo-random number sequence for any given seed" in {
		forAll { (rng: RNG, i: Int) => 
			val n = (i % 100).abs + 1
			sequencedInts(n)(rng)._1 === sequencedInts(n)(rng)._1
		}
	}
	it should "correctly simulate a dice roll" in {
		forAll { (rng: RNG) => rollDie(rng)._1 >= 1 && rollDie(rng)._1 <= 6 }
	}
	

	"Machine" should "ignore all inputs if out of candy" in {
		val m = Machine(false, 0, 0)
		val m2 = m.copy(locked = true)
		
		simulateMachine(turn).run(m)._2 === m
		simulateMachine(coin).run(m)._2 === m
		simulateMachine(turn).run(m2)._2 === m2
		simulateMachine(coin).run(m2)._2 === m2
	}
	it should "unlock if a coin is inserted and it contains candy" in {
		val m = Machine(true, 5, 0)
		val dispensedM = m.copy(coins = m.coins + 1)

		simulateMachine(coin).run(m)._2 === dispensedM
	}
	it should "dispense a candy if it has been unlocked and turned" in {
		val m = Machine(false, 10, 1)
		val dispensedM = m.copy(candies = m.candies - 1)

		simulateMachine(turn).run(m)._2 === dispensedM
	}
	it should "turn off after becoming sold out of candy" in {
		val m = Machine(true, 5, 0)
		val inputs: List[Input] = List.fill(5)(coin ++ turn).flatten

		simulateMachine(inputs).run(m) === simulateMachine(inputs ++ inputs).run(m)
	}
	it should "always result in the same end state for any given set of operations" in {
		forAll { (m: Machine, inputs: List[Input]) => 
			simulateMachine(inputs).run(m) === simulateMachine(inputs).run(m)
		}
	} //TODO: can prolly make these better

}

object StateGenerator { 
	implicit def arbMachine(implicit I: Arbitrary[Int], B: Arbitrary[Boolean]): Arbitrary[Machine] = {
    lazy val machine: Gen[Machine] = Gen.lzy(Gen.sized { size =>
        for{
        	l <- B.arbitrary
        	candies <- I.arbitrary
        	coins <- I.arbitrary
        } yield Machine(l, candies, coins)
    })
  	Arbitrary(machine)
  }

  implicit def arbInput: Arbitrary[List[Input]] =
  		Arbitrary(Gen.containerOf[List, Input](Gen.oneOf(Coin, Turn)))

  implicit def arbRand[A](implicit A: Arbitrary[A]): Arbitrary[Rand[A]] =
  	Arbitrary(A.arbitrary flatMap (a => RNG.unit(a)))

  implicit def arbRNG(implicit L: Arbitrary[Long]): Arbitrary[RNG] = 
  	Arbitrary(L.arbitrary flatMap (l => Simple(l)))
}
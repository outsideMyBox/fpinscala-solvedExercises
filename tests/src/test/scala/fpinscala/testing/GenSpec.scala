package fpinscala.testing

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.matcher.DataTables
import fpinscala.state.RNG
import fpinscala.state.State

@RunWith(classOf[JUnitRunner])
class GenSpec extends Specification with DataTables {

  val seedInt = 340707234

  type Rand[+A] = RNG => (A, RNG)

  /**
   * Generate a list of 'count' results.
   */
  private def sequenceFromTest[A](count: Int)(rng: RNG)(f: Rand[A]): (List[A], RNG) = {
    @annotation.tailrec
    def go(c: Int, acc: List[A], rng: RNG): (List[A], RNG) =
      if (c == 0)
        (acc, rng)
      else {
        val (a, r) = f(rng)
        go(c - 1, a :: acc, r)
      }
    go(count, List(), rng)
  }

  "The following exercises should be correct" >> {

    "Exercise 8.3: &&" in {
      import fpinscala.testing.firstIteration.Prop
      val propFalse = new Prop { def check = false }
      val propTrue = new Prop { def check = true }

      "propA" || "proB" || "expected result" |
        propFalse !! propFalse !! false |
        propFalse !! propTrue !! false |
        propTrue !! propFalse !! false |
        propTrue !! propTrue !! true |> {
          (propA, propB, expected) =>
            (propA && propB).check must_== expected
        }
    }

    "Exercise 8.4: choose" in {
      import java.lang.Integer.{ MIN_VALUE, MAX_VALUE }
      "start" || "stopExclusive" |
        -2 !! 2 |
        MIN_VALUE !! MIN_VALUE + 2 |
        MAX_VALUE - 2 !! MAX_VALUE |> {
          (start, stopExclusive) =>
            val gen = Gen.choose(start, stopExclusive)
            val seed = RNG.Simple(seedInt)
            val generatedInts = sequenceFromTest(1000)(seed)(gen.sample.run)._1
            (generatedInts.filter(_ < start) must be empty) and
              (generatedInts.filter(_ >= stopExclusive) must be empty)
        }
    }

    "Exercise 8.5: unit" in {
      val gen = Gen.unit("unit")
      val seed = RNG.Simple(seedInt)
      val generatedUnits = sequenceFromTest(1000)(seed)(gen.sample.run)._1
      generatedUnits.forall(_ == "unit") must beTrue
    }

    "Exercise 8.5: boolean" in {
      val gen = Gen.boolean
      val seed = RNG.Simple(seedInt)
      val generatedBooleans = sequenceFromTest(1000)(seed)(gen.sample.run)._1
      (generatedBooleans must contain(false)) and (generatedBooleans must contain(true))
    }

    "Exercise 8.5: listOfN" in {
      val start = -2
      val stopExclusive = 2
      val seed = RNG.Simple(seedInt)
      val listSize = 100
      val gen = Gen.listOfN(listSize, Gen.choose(start, stopExclusive))
      val ints = gen.sample.run(seed)._1
      (ints must haveSize(listSize)) and
        (ints.filter(_ < start) must be empty) and
        (ints.filter(_ >= stopExclusive) must be empty)
    }
    
    "Exercise 8.6: flatMap" in {
      todo
    }

    "Exercise 8.6: listOfN" in {
      todo
    }    
    
  }
}
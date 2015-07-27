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
      "start" || "stopExclusive" || "expected" |
        -2 !! 2 !! List(-1, 0, 0, 0) |
        MIN_VALUE !! MIN_VALUE + 2 !! List(-2147483647, -2147483648, -2147483648, -2147483648) |
        MAX_VALUE - 2 !! MAX_VALUE !! List(2147483646, 2147483645, 2147483645, 2147483645) |> {
          (start, stopExclusive, expected) =>
            val gen = Gen.choose(start, stopExclusive)
            val seed = RNG.Simple(seedInt)
            val (ints1, rng1) = sequenceFromTest(4)(seed)(gen.sample.run)
            val ints2 = sequenceFromTest(1000)(rng1)(gen.sample.run)._1
            (ints1 must_== expected) and
              (ints2 forall (elt => elt >= start && elt < stopExclusive) must beTrue)
        }
    }

    "Exercise 8.5: unit" in {
      val gen = Gen.unit("unit")
      val seed = RNG.Simple(seedInt)
      val (units1, rng) = sequenceFromTest(10)(seed)(gen.sample.run)
      val units2 = sequenceFromTest(10)(rng)(gen.sample.run)._1
      units1.union(units2) forall (_ == "unit") must beTrue
    }

    "Exercise 8.5: boolean" in {
      val gen = Gen.boolean
      val seed = RNG.Simple(seedInt)
      val (booleans1, rng) = sequenceFromTest(3)(seed)(gen.sample.run)
      val booleans2 = sequenceFromTest(4)(rng)(gen.sample.run)._1
      (booleans1 must_== List(false, true, true)) and
        (booleans2 must_== List(true, true, false, false))
    }

    "Exercise 8.5: listOfN" in {
      val start = -2
      val stopExclusive = 2
      val seed = RNG.Simple(seedInt)
      val listSize = 100
      val gen = Gen.listOfN(listSize, Gen.choose(start, stopExclusive))
      val (ints1, rng) = gen.sample.run(seed)
      val ints2 = gen.sample.run(rng)._1
      val ints = ints1 union ints2
      (ints must haveSize(listSize * 2)) and
        (ints.filter(_ < start) must be empty) and
        (ints.filter(_ >= stopExclusive) must be empty)
    }

    "Exercise 8.6: flatMap" in {
      val nbLetterGen = Gen.choose(1, 10)
      val f = (nbInts: Int) => Gen.listOfN(nbInts, Gen.choose(10, 15))
      val wordGen = nbLetterGen.flatMap(f)
      val seed = RNG.Simple(seedInt)
      val (res1, rng1) = wordGen.sample.run(seed)
      val (res2, rng2) = wordGen.sample.run(rng1)
      val res3 = wordGen.sample.run(rng2)._1
      (res1 must_== List(10, 14, 14)) and
        (res2 must_== List(13, 12, 11, 12, 12, 12, 14)) and
        (res3 must_== List(10, 10, 11))
    }

    "Exercise 8.6: listOfN" in {
      val booleanGen = Gen.boolean
      val gen = booleanGen.listOfN(Gen.choose(0, 5))
      val seed = RNG.Simple(seedInt)
      val lists = sequenceFromTest(6)(seed)(gen.sample.run)._1
      lists must_== List(List(), List(false, false, false), List(false, true), List(true, true, false), List(), List(true, false, false))
    }

    "Exercise 8.7: union" in {
      val nbTests = 1000
      val genF = Gen.unit(false)
      val genT = Gen.unit(true)
      val gen = Gen.union(genF, genT)
      val seed = RNG.Simple(seedInt)
      val booleans = sequenceFromTest(nbTests)(seed)(gen.sample.run)._1
      val percentError = 2
      val margin = nbTests * percentError / 100
      val expected = nbTests / 2
      (booleans.filter(_ == false).size must beGreaterThan(expected - margin)) and
        (booleans.filter(_ == true).size must beGreaterThan(expected - margin))
    }

    "Exercise 8.8: weighted" in {
      "weightFalse" || "weightTrue" |
        1 !! 1 |
        2 !! 1 |
        1 !! 3 |
        1 !! 3 |
        1 !! 10 |
        100 !! 1 |
        1 !! 0 |
        0 !! 1 |
        10 !! 10 |> {
          (falseWeight, trueWeight) =>
            val nbTests = 1000
            val percentError = 3
            val margin = nbTests * percentError / 100
            val falseGen = Gen.unit(false)
            val trueGen = Gen.unit(true)
            val weightedGen = Gen.weighted((falseGen, falseWeight), (trueGen, trueWeight))
            val seed = RNG.Simple(seedInt)
            val booleans = sequenceFromTest(nbTests)(seed)(weightedGen.sample.run)._1
            val falseNb = booleans.filter(_ == false).size
            val trueNb = booleans.filter(_ == true).size
            val expectedFalseNb = falseWeight * nbTests / (falseWeight + trueWeight)
            val expectedTrueNb = trueWeight * nbTests / (falseWeight + trueWeight)

            (falseNb must beGreaterThan(expectedFalseNb - margin)) and
              (falseNb must beLessThan(expectedFalseNb + margin)) and
              (trueNb must beGreaterThan(expectedTrueNb - margin)) and
              (trueNb must beLessThan(expectedTrueNb + margin))
        }
    }

  }
}
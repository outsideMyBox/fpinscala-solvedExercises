package fpinscala.state

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class StateSpec extends Specification with DataTables {

  // TODO: Update tests to test limits and be more rigorous about the different cases (e.g. test real values).

  type Rand[+A] = RNG => (A, RNG)

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

    "Exercise 6.1: non negative int." in {
      val seed = RNG.Simple(340707234)
      val (i1, rng1) = RNG.nonNegativeInt(seed)
      val (i2, rng2) = RNG.nonNegativeInt(seed)
      val (i3, rng3) = RNG.nonNegativeInt(rng2)

      (i1 must_== i2) and (i2 mustNotEqual i3)
      val results = sequenceFromTest(1000)(seed)(RNG.nonNegativeInt)._1
      (results.filter(_ < 0).isEmpty must_== true) and (results.distinct.isEmpty must_== false)
    }

    "Exercise 6.2: double between 0 and 1." in {
      val seed = RNG.Simple(340707234)
      val results = sequenceFromTest(1000)(seed)(RNG.double)._1
      (results.filter(x => (x >= 1) || (x < 0)).isEmpty must_== true) and (results.distinct.isEmpty must_== false)
    }

    "Exercise 6.3: intDouble." in {
      val seed = RNG.Simple(340707234)
      val results = sequenceFromTest(1000)(seed)(RNG.intDouble)._1
      val (ints, doubles) = results.unzip { case ((a, b)) => (a, b) }
      (ints.distinct.isEmpty must_== false) and (doubles.distinct.isEmpty must_== false)
    }

    "Exercise 6.3: doubleInt." in {
      val seed = RNG.Simple(340707234)
      val results = sequenceFromTest(1000)(seed)(RNG.doubleInt)._1
      val (ints, doubles) = results.unzip { case ((a, b)) => (a, b) }
      (ints.distinct.isEmpty must_== false) and (doubles.distinct.isEmpty must_== false)
    }

    "Exercise 6.3: double3." in {
      val seed = RNG.Simple(340707234)
      val results = sequenceFromTest(50)(seed)(RNG.double3)._1
      val (d1, d2, d3) = results.unzip3 { case ((a, b, c)) => (a, b, c) }
      (d1.distinct.isEmpty must_== false) and (d2.distinct.isEmpty must_== false) and
        (d3.distinct.isEmpty must_== false) and (d1 mustNotEqual d2) and (d1 mustNotEqual d3)
    }

    "Exercise 6.4: list of random integers." in {
      val seed = RNG.Simple(340707234)
      val results = RNG.ints(1000)(seed)._1
      (results.length must_== 1000) and (results.distinct.isEmpty must_== false)
    }

    "Exercise 6.5: double between 0 and 1 with map." in {
      val seed = RNG.Simple(340707234)
      val results = sequenceFromTest(1000)(seed)(RNG._double)._1
      (results.filter(x => (x >= 1) || (x < 0)).isEmpty must_== true) and (results.distinct.isEmpty must_== false)
    }

    "Exercise 6.6: map2." in {
      val seed = RNG.Simple(340707234)
      val ra = RNG.int
      val rb = RNG._double
      val rc = RNG.map2(ra, rb)((i, d) => i.toString + "/" + d.toString)
      rc(seed)._1 must_== "36696638/0.6034165667369962"
    }

    "Exercise 6.7: sequence." in {
      val seed = RNG.Simple(340707234)
      def rand1 = RNG.int
      def rand2 = RNG.unit(100)
      def rand3 = RNG.unit(200)
      def rand4 = RNG.int
      val resultEmptyList = RNG.sequence(List())(seed)._1

      val list = List(rand1, rand2, rand3, rand4)
      val resultList = RNG.sequence(list)(seed)._1
      (resultEmptyList must_== List.empty) and (resultList must_== List(36696638, 100, 200, 1295827210))
    }

    "Exercise 6.7: ints with sequence." in {
      val seed = RNG.Simple(340707234)
      val results = RNG._ints(5)(seed)._1
      results must_== List(36696638, 1295827210, -1091999955, 919578349, 1438262205)
    }

    "Exercise 6.8: flatMap." in {
      todo
    }

    "Exercise 6.8: nonNegativeLessThan with flatMap." in {
      val seed = RNG.Simple(340707234)
      val max = 5
      val (i1, rng1) = RNG.nonNegativeLessThan(max)(seed)
      val (i2, rng2) = RNG.nonNegativeLessThan(max)(seed)
      val (i3, rng3) = RNG.nonNegativeLessThan(max)(rng2)
      val l = List(1, 2)
      (i1 must_== i2) and (i2 mustNotEqual i3)
      val results = sequenceFromTest(1000)(seed)(RNG.nonNegativeLessThan(max))._1
      (results.filter(_ > max).isEmpty must_== true) and (results.distinct.isEmpty must_== false)
    }

    "Exercise 6.9: map via flatMap." in {
      val seed = RNG.Simple(340707234)
      val s = RNG.int
      def f(i: Int) = i.toString
      RNG._map(s)(f)(seed)._1 must_== "36696638"
    }

    "Exercise 6.9: map2 via flatMap." in {
      val seed = RNG.Simple(340707234)
      val ra = RNG.int
      val rb = RNG._double
      val rc = RNG._map2(ra, rb)((i, d) => i.toString + "/" + d.toString)
      rc(seed)._1 must_== "36696638/0.6034165667369962"
    }

    case class CA(val a: String)
    case class CB(val b: String)

    case class CS(val id: Int) {
      val a = CA("vala=" + id)
      val b = CB("valb=" + (id + 10))
    }

    "Exercise 6.10: unit for State." in {
      todo
    }

    "Exercise 6.10: map for State." in {
      def run(s: CS): (CA, CS) = (s.a, CS(s.id + 1))
      val state = State(run)
      val s0 = CS(0)
      val (a0, s1) = state.run(s0)
      val (a1, s2) = state.run(s1)
      val (a2, s3) = state.run(s2)

      def f(a: CA) = CB(a.a.toUpperCase())
      val stateMap = state.map(f)
      val (b0, sm1) = stateMap.run(s0)
      val (b1, sm2) = stateMap.run(sm1)
      val (b2, sm3) = stateMap.run(sm2)

      ((a0, a1, a2) must_== (CA("vala=0"), CA("vala=1"), CA("vala=2"))) and
        ((b0, b1, b2) must_== (CB("VALA=0"), CB("VALA=1"), CB("VALA=2")))

    }

    case class CC(val b: String)

    "Exercise 6.10: map2 for State." in {
      def run(s: CS): (CA, CS) = (s.a, CS(s.id + 1))
      val state: State[CS, CA] = State(run)
      val s0 = CS(0)

      def runb(s: CS): (CB, CS) = (s.b, CS(s.id))
      def f(a: CA, b: CB): CC = CC(a.a + "/" + b.b)
      val sb: State[CS, CB] = State(runb)

      // def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
      val stateMap2: State[CS, CC] = state.map2(sb)(f)
      val (c0, sm1) = stateMap2.run(s0)
      val (c1, sm2) = stateMap2.run(sm1)
      val (c2, sm3) = stateMap2.run(sm2)
      (c0, c1, c2) must_== (CC("vala=0/valb=11"), CC("vala=1/valb=12"), CC("vala=2/valb=13"))
    }

    "Exercise 6.10: flatMap for State." in {
      def run(s: CS): (CA, CS) = (s.a, CS(s.id + 1))
      val state: State[CS, CA] = State(run)
      val s0 = CS(1)

      def f(a: CA): State[CS, CB] = {
        State(s => if (a.a == 2) (s.b, CS(s.id * 2)) else (s.b, CS(s.id * -2)))
      }
      val stateFlatMap: State[CS, CB] = state.flatMap(f)
      val (c0, sm1) = stateFlatMap.run(s0)
      val (c1, sm2) = stateFlatMap.run(sm1)
      val (c2, sm3) = stateFlatMap.run(sm2)
      (c0, c1, c2) must_== (CB("valb=12"), CB("valb=7"), CB("valb=17"))
    }

    "Exercise 6.10: sequence for State." in {
      todo
    }

    "Exercise 6.11: candy machine." in {
      "initial state : Machine(locked, candies, coins)" || "inputs" || "end state : ((coins, candies), Machine)" |
        Machine(true, 1, 2) !! Nil !! ((2, 1), Machine(true, 1, 2)) |
        Machine(false, 0, 0) !! Nil !! ((0, 0), Machine(false, 0, 0)) |
        Machine(true, 1, 2) !! List(Coin) !! ((3, 1), Machine(false, 1, 3)) |
        Machine(true, 0, 2) !! List(Coin) !! ((2, 0), Machine(true, 0, 2)) |
        Machine(true, 1, 2) !! List(Turn) !! ((2, 1), Machine(true, 1, 2)) |
        Machine(false, 2, 4) !! List(Coin) !! ((4, 2), Machine(false, 2, 4)) |
        Machine(false, 2, 4) !! List(Turn) !! ((4, 1), Machine(true, 1, 4)) |
        Machine(true, 2, 4) !! List(Coin, Coin, Coin) !! ((5, 2), Machine(false, 2, 5)) |
        Machine(false, 2, 4) !! List(Turn, Turn, Turn) !! ((4, 1), Machine(true, 1, 4)) |
        Machine(true, 2, 4) !! List(Coin, Coin, Coin, Turn, Turn, Turn) !! ((5, 1), Machine(true, 1, 5)) |
        Machine(true, 2, 4) !! List(Coin, Coin, Coin, Turn, Turn, Turn) !! ((5, 1), Machine(true, 1, 5)) |
        Machine(true, 2, 10) !! List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn) !! ((12, 0), Machine(true, 0, 12)) |
        Machine(true, 4, 5) !! List(Coin, Turn, Coin, Turn, Coin, Turn) !! ((8, 1), Machine(true, 1, 8)) |> {
          (initialState, inputs, finalState) =>
            Candy.simulateMachine(inputs).run(initialState) must_== finalState
          //State.simulateMachineFromBook(inputs).run(initialState) must_== finalState
        }
    }
  }
}
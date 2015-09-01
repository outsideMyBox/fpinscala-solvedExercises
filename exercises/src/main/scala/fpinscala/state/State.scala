package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1: non negative int.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    val i2 = if (i == Int.MinValue) -(i + 1) else Math.abs(i)
    (i2, rng2)
  }

  // Exercise 6.2: double between 0 and 1.
  def double(rng: RNG): (Double, RNG) = {
    val (i2, rng2) = nonNegativeInt(rng)
    (i2 / (Int.MaxValue.toDouble + 1), rng2)
  }

  // Exercise 6.3: intDouble.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  // Exercise 6.3: doubleInt.
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  // Exercise 6.3: double3.
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // Exercise 6.4: list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(n: Int, list: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n == 0) {
        (list, rng)
      } else {
        val (i, r) = rng.nextInt
        go(n - 1, i :: list, r)
      }
    }
    go(count, Nil, rng)
  }

  // Exercise 6.5: double between 0 and 1 with map.
  def _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6: map2.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rnga)
    (f(a, b), rngb)
  }

  // Exercise 6.7: sequence.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val z = (List[A](), rng)
    val (a, r) = fs.foldLeft(z) { (acc: (List[A], RNG), elt: Rand[A]) =>
      val (listA, r1) = acc
      val (a, r2) = elt(r1)
      (a :: listA, r2)
    }
    (a.reverse, r)
  }

  // Exercise 6.7: ints with sequence.
  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Exercise 6.8: flatMap.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  // Exercise 6.8: nonNegativeLessThan with flatMap.
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    def g(i: Int): Rand[Int] = {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }
    flatMap(nonNegativeInt)(g)
  }

  // Exercise 6.9: map via flatMap.
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // Exercise 6.9: map2 via flatMap.
  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10: map for State.
  /*  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })
  }*/
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  // Exercise 6.10: map2 for State.
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  // Exercise 6.10: flatMap for State.
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
  type Rand[A] = State[RNG, A]

  // Exercise 6.10: unit for State.
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 6.10: sequence for State.
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = ???

}

object Candy {
  // Exercise 6.11: candy machine.
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    @annotation.tailrec
    def go(inputs: List[Input])(m: Machine): ((Int, Int), Machine) =
      inputs match {
        case Coin :: t if (m.locked && m.candies > 0) =>
          go(t)(Machine(false, m.candies, m.coins + 1))
        case Turn :: t if (!m.locked) =>
          go(t)(Machine(true, m.candies - 1, m.coins))
        case _ :: t => go(t)(m)
        case Nil    => ((m.coins, m.candies), m)
      }

    State(go(inputs))
  }
}
package fpinscala.state


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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1: non negative int.
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  // Exercise 6.2: double between 0 and 1.
  def double(rng: RNG): (Double, RNG) = ???

  // Exercise 6.3: intDouble.
  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  // Exercise 6.3: doubleInt.
  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  // Exercise 6.3: double3.
  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  // Exercise 6.4: list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  // Exercise 6.5: double between 0 and 1 with map.
  def _double: Rand[Double] = ???
  
  // Exercise 6.6: map2.
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  // Exercise 6.7: sequence.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???
  
  // Exercise 6.7: ints with sequence.
  def _ints(count: Int): Rand[List[Int]] = ???

  // Exercise 6.8: flatMap.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
  
  // Exercise 6.8: nonNegativeLessThan with flatMap.
  def nonNegativeLessThan(n: Int): Rand[Int] = ???
  
  // Exercise 6.9: map via flatMap.
  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = ???
  
  // Exercise 6.9: map2 via flatMap.
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???
}

case class State[S,+A](run: S => (A, S)) {
  // Exercise 6.10: map for State.
  def map[B](f: A => B): State[S, B] = ???

  // Exercise 6.10: map2 for State.
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

  // Exercise 6.10: flatMap for State.
  def flatMap[B](f: A => State[S, B]): State[S, B] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  
  // Exercise 6.10: unit for State.
  def unit[S, A](a: A): State[S, A] = ???
  
  // Exercise 6.10: sequence for State.
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = ???
 
}

object Candy {
  // Exercise 6.11: candy machine.
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
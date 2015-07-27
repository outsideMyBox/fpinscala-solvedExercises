package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

package firstIteration {

  trait Prop {
    def check: Boolean

    // Exercise 8.3: &&
    def &&(p: Prop): Prop = ???
  }

}

//trait Prop {
//  def check: Boolean
//
//  def &&(p: Prop): Prop = ???
//
//}
//
//object Prop {
//  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
//}

object Gen {
  // Exercise 8.4: choose
  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

  // Exercise 8.5: unit  
  def unit[A](a: => A): Gen[A] = ???

  // Exercise 8.5: boolean
  val boolean: Gen[Boolean] = ???

  // Exercise 8.5: listOfN
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???

  // Exercise 8.7: union
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

  // Exercise 8.8: weighted
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

  // Exercise 8.12: listOf
  def listOf[A](g: Gen[A]): SGen[List[A]] = ???  

  // Exercise 8.13: listOf1
  def listOf1[A](g: Gen[A]): SGen[List[A]] = ???

  // Exercise 8.13: maxProp with listOf1
  val maxProp1:Prop = ???
    
  // Exercise 8.14: sortedProp
  val sortedProp:Prop = ???
  
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[A, B](f: A => B): Gen[B] = ???

  // Exercise 8.6: flatMap  
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

  // Exercise 8.6: listOfN
  def listOfN(size: Gen[Int]): Gen[List[A]] = ???

  // Exercise 8.10: unsized
  def unsized: SGen[A] = ???
  
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Exercise 8.9: &&
  def &&(p: Prop): Prop = ???

  // Exercise 8.9: ||
  def ||(p: Prop): Prop = ???

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x               => x
      }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  val p2 = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2))(ES) get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

}

//trait SGen[+A] {
//
//}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  // Exercise 8.11: convenience functions (map)
  def map[B](f: A => B): SGen[B] = ???
  
  // Exercise 8.11: convenience functions (flatMap)
  def flatMap[B](f: A => Gen[B]): SGen[B] = ???  
  
  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}


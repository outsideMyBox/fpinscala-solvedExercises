package fpinscala.errorhandling

import scala.{ Option => _, Some => _, Either => _, _ } // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.annotation.tailrec

sealed trait Option[+A] {

  // Exercise 4.1: map for Option
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  // Exercise 4.1: flatMap for Option (without pattern matching).
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  // Exercise 4.1: flatMap for Option (with pattern matching).
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None    => None
  }

  // Exercise 4.1: getOrElse for Option.
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(b) => b
    case None    => default
  }

  // Exercise 4.1: orElse for Option (without pattern matching).
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(sa => Some(sa)).getOrElse(ob)

  // Exercise 4.1: orElse for Option (with pattern matching).
  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None  => ob
    case someB => someB

  }

  // Exercise 4.1: filter for Option (with pattern matching).
  def filter(f: A => Boolean): Option[A] = this match {
    case sa @ Some(a) if f(a) => sa
    case _                    => None
  }

  // Exercise 4.1: filter for Option (without pattern matching).
  def filter_1(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2: variance.
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) map (m => xs.map(x => math.pow(x - m, 2)).sum / xs.length)

  // Exercise 4.3: map2.
  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa flatMap (a => ob.map(b => f(a, b)))

  // Exercise 4.4: sequence.
  def sequence1[A](loa: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def sequence0(l: List[Option[A]], acc: List[A]): Option[List[A]] = {
      l match {
        case Nil             => Some(acc)
        case None :: tail    => None
        case Some(h) :: tail => sequence0(tail, h :: acc)
      }
    }
    sequence0(loa, Nil) map (_.reverse)
  }

  def sequence2[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil       => Some(Nil)
    case hOpt :: t => hOpt flatMap (h => sequence(t).map(h :: _))
  }

  def sequence[A](loa: List[Option[A]]): Option[List[A]] =
    loa.foldRight[Option[List[A]]](Some(Nil))((optA, optLa) => optA.flatMap(a => optLa.map(la => a :: la)))

  // Exercise 4.5: traverse (straightforward).
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)

  // Exercise 4.5: traverse (implementation that only looks at the sequence once)
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a, optLb) => map2(f(a), optLb)(_ :: _))
  //a.foldRight[Option[List[B]]](Some(Nil))((a, optLb) => f(a).flatMap(a => optLb.map(la => a:: la) ))

  // Exercise 4.5: sequence in terms of traverse.
  def sequenceViaTraverse[A](l: List[Option[A]]): Option[List[A]] =
    traverse_1(l)(a => a)
}
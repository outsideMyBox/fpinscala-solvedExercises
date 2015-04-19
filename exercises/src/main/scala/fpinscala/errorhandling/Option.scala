package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  
  // Exercise 4.1: map for Option
  def map[B](f: A => B): Option[B] = ???

  // Exercise 4.1: flatMap for Option (without pattern matching).
  def flatMap[B](f: A => Option[B]): Option[B] = ???
  
  // Exercise 4.1: flatMap for Option (with pattern matching).
  def flatMap_1[B](f: A => Option[B]): Option[B] = ???
  
  // Exercise 4.1: getOrElse for Option.
  def getOrElse[B>:A](default: => B): B = ???

  // Exercise 4.1: orElse for Option (without pattern matching).
  def orElse[B>:A](ob: => Option[B]): Option[B] = ???
  
  // Exercise 4.1: orElse for Option (with pattern matching).
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = ???

  // Exercise 4.1: filter for Option (with pattern matching).
  def filter(f: A => Boolean): Option[A] = ???
  
  // Exercise 4.1: filter for Option (without pattern matching).
  def filter_1(f: A => Boolean): Option[A] = ???
  
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
    
  // Exercise 4.2: variance.
  def variance(xs: Seq[Double]): Option[Double] = ???

  // Exercise 4.3: map2.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  // Exercise 4.4: sequence.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  // Exercise 4.5: traverse (straightforward).
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
  
  // Exercise 4.5: traverse (implementation that only looks at the sequence once)
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
  
  // Exercise 4.5: sequence in terms of traverse.
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = ???
}
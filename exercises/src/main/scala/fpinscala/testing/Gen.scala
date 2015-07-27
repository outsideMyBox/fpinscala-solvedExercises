package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

package firstIteration {

  trait Prop {
    def check: Boolean

    // Exercise 8.3: &&
    def &&(p: Prop): Prop = ???
  }

}

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop = ???

}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

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

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[A, B](f: A => B): Gen[B] = ???

  // Exercise 8.6: flatMap  
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

  // Exercise 8.6: listOfN
  def listOfN(size: Gen[Int]): Gen[List[A]] = ???
}

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}




trait SGen[+A] {

}


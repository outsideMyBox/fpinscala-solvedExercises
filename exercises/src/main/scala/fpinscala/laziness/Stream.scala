package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  // Exercise 5.1: toList.
  def toList:List[A] = ???
  
  // Exercise 5.2: take.
  def take(n: Int): Stream[A] = ???

  // Exercise 5.2: drop.
  def drop(n: Int): Stream[A] = ???

  // Exercise 5.3: takeWhile.
  def takeWhile(p: A => Boolean): Stream[A] = ???

  // Exercise 5.4: forAll.
  def forAll(p: A => Boolean): Boolean = ???

  // Exercise 5.5: takeWhile via foldRight.
  def takeWhile_1(p: A => Boolean): Stream[A] = ???
  
  // Exercise 5.6: headOption Via foldRight.
  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  
  // Exercise 5.7: map.
  def map[B](f: A => B): Stream[B] = ???
    
  //Exercise 5.7: filter.
  def filter(f: A => Boolean): Stream[A] = ???
  
  // Exercise 5.7: append.
  def append[B>:A](s: => Stream[B]): Stream[B] = ???

  // Exercise 5.7: flatMap.
  def flatMap[B](f: A => Stream[B]): Stream[B] = ???
  
  // Exercise 5.13: map via unfold.
  def mapViaUnfold[B](f: A => B): Stream[B] = ???
  
  // Exercise 5.13: take via unfold.
  def takeViaUnfold(n: Int): Stream[A] = ???
  
  // Exercise 5.13: takeWhile via unfold.
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = ???
  
  // Exercise 5.13: zipWith via unfold.
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = ???
  
  // Exercise 5.13: zipAll via unfold.
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???  
  
  // Exercise 5.14: startWith via unfold.
  def startsWith[B](s: Stream[B]): Boolean = ???
  
  // Exercise 5.15: tails via unfold.
  def tails: Stream[Stream[A]] = ???
  
  // Exercise 5.16: scanRight.
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  // Exercise 5.8: constant.
  def constant[A](a: A): Stream[A] = ???
  
  // Exercise 5.9: from.
  def from(n: Int): Stream[Int] = ???
  
  // Exercise 5.10: fibs.
  def fibs: Stream[Int] = ???
  
  // Exercise 5.11: unfold.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
  
  // Exercise 5.11: unfold via fold
  //def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  // Exercise 5.11: unfold via fold
  //def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
  // def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
  
  // Exercise 5.12: fibs, from, constant , and ones in terms of unfold.
  def fibsViaUnfold: Stream[Int] = ???
  
  def fromViaUnfold(n: Int):Stream[Int] = ???
  
  def constantViaUnfold[A](a: A):Stream[A] = ???
  
  def onesViaUnfold: Stream[Int] = ???
  
  
}
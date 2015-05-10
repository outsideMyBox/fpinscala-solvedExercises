package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.1: result of match.
  def resultOfMatchExpression: Int = 3

  // Exercise 3.2: tail.
  // Use the message "tail of empty list" for your exception.
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil        => sys.error("tail of empty list")
  }

  // Exercise 3.3: setHead.
  // Use the message "setHead on empty list" for your exception.
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil        => sys.error("setHead on empty list")
  }

  // Exercise 3.4: drop.
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case _          => l
    }

  // Exercise 3.5: dropWhile.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  // Exercise 3.6: init.
  // Use the message "init of empty list" for your exception.
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
    case Nil          => sys.error("init of empty list")
  }

  // Exercise 3.9: length with foldRight.
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  // Exercise 3.10: foldLeft.
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(a, t) => foldLeft(t, f(z, a))(f)
    case Nil        => z
  }

  // Exercise 3.11: sum , product and length with foldLeft.
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((l, _) => l + 1)

  // Exercise 3.12: reverse.
  //  def reverse[A](l: List[A]): List[A] = {
  //    def setLast(list: List[A], last: A): List[A] = list match {
  //      case Nil          => Cons(last, Nil)
  //      case Cons(h, Nil) => Cons(h, Cons(last, Nil))
  //      case Cons(h, t)   => Cons(h, setLast(t, last))
  //    }
  //    l match {
  //      case Nil        => Nil
  //      case Cons(h, t) => setLast(reverse(t), h)
  //    }
  //  }
  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def reverse0(list: List[A], acc: List[A]): List[A] = list match {
      case Nil        => acc
      case Cons(h, t) => reverse0(t, Cons(h, acc))
    }
    reverse0(l, Nil)
  }

  def reverseWithFold[A](l: List[A]): List[A] = foldLeft(l, List[A]())((b, a) => Cons(a, b))

  // Exercise 3.13: foldRight in terms of foldLeft.
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  // Exercise 3.13: foldLeft in terms of foldRight.
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    // From book
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z) // From book

  // Exercise 3.14: append in terms of either foldLeft or foldRight.
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((elt, listToAppend) => Cons(elt, listToAppend))

  def appendViaFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2)((listToAppend, elt) => Cons(elt, listToAppend))

  // Exercise 3.15: concatenation.
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())(appendViaFoldLeft)

  // Exercise 3.16: add one.
  def add1(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, List[Int]())((int, b) => Cons(int + 1, b))

  // Exercise 3.17: double to string.
  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, List[String]())((double, b) => Cons(double.toString, b))

  // Exercise 3.18: map.
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, List[B]())((a, b) => Cons(f(a), b))

  // Exercise 3.19: filter.
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  // Exercise 3.20 flatMap.
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // Exercise 3.21: filter with flatMap.
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((a) => if (f(a)) List(a) else Nil)

  // Exercise 3.22: add elts of two lists.
  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  // Exercise 3.23: zipWith.
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // Exercise 3.24: hasSubsequence.

  def isPrefix[A](list: List[A], prefix: List[A]): Boolean = (list, prefix) match {
    case (_, Nil)                                 => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => isPrefix(t1, t2)
    case _                                        => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil)        => true
    case (Nil, Cons(_, _)) => false
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if (h1 == h2)
        isPrefix(t1, t2)
      else
        hasSubsequence(t1, sub)
  }
}

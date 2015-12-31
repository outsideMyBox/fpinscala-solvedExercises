package fpinscala.laziness

import Stream._
import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1: toList.
  def toListSimple: List[A] = this match {
    case Empty      => List()
    case Cons(h, t) => h() :: t().toListSimple
  }

  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def toList0(stream: Stream[A]): List[A] = {
      stream match {
        case Cons(h, t) => {
          buf += h()
          toList0(t())
        }
        case _ => buf.toList
      }
    }
    toList0(this)
  }

  // Exercise 5.2: take.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case _                    => empty
  }

  // Exercise 5.2: drop.
  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  // Exercise 5.3: takeWhile.
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) =>
        val hVal = h()
        if (p(hVal)) cons(hVal, t().takeWhile(p)) else empty
      case _ => empty
    }
  }

  // Exercise 5.4: forAll.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  // Exercise 5.5: takeWhile via foldRight.
  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  // Exercise 5.6: headOption Via foldRight.
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // Exercise 5.7: map.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  //Exercise 5.7: filter.
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  // Exercise 5.7: append.
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  // Exercise 5.7: flatMap.
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // Exercise 5.13: map via unfold.
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  // Exercise 5.13: take via unfold.
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  // Exercise 5.13: takeWhile via unfold.
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) =>
        val hVal = h()
        if (f(hVal)) Some(hVal, t()) else None
      case _ => None
    }

  // Exercise 5.13: zipWith via unfold.
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  // Exercise 5.13: zipAll via unfold.
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2))        => Some(((None, Some(h2())), (empty, t2())))
      case (Cons(h1, t1), Empty)        => Some(((Some(h1()), None), (t1(), empty)))
      case _                            => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))  
  
   // Exercise 5.14: startWith.
  def startsWith1[B](s: Stream[B]): Boolean =
    (this, s) match {
      case (_, Empty) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith1(t2())
      case _ => false
    }

  // Exercise 5.15: tails via unfold.
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case stream @ Cons(h, t) => Some((stream, t()))
      case _                   => None
    } append Stream(empty)

  // Exercise 5.16: scanRight.
  // From book
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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
  def constant1[A](a: A): Stream[A] = cons(a, constant1(a))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5.9: from.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10: fibs.
  def fibs: Stream[Int] = {
    def fibs0(nm2: Int, nm1: Int): Stream[Int] = {
      val n = nm2 + nm1
      cons(n, fibs0(nm1, n))
    }
    cons(0, cons(1, fibs0(0, 1)))
  }

  // Exercise 5.11: unfold.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _            => empty
    }

  // Exercise 5.12: fibs, from, constant , and ones in terms of unfold.
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (nm2, nm1) => Some((nm2, (nm1, nm2 + nm1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)

}
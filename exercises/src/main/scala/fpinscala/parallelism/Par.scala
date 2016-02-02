package fpinscala.parallelism

import java.util.concurrent._
import scala.annotation.tailrec

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })
    
  // Exercises 7.3: map2 that respects the contract of timeouts on Future.
  def map2WithTimeOut[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = 
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      TimeSpentAwareFuture(af, bf, f)
    }
    
  case class TimeSpentAwareFuture[A,B,C](af:Future[A], bf:Future[B], f: (A, B) => C) extends Future[C] {
    var resultOpt: Option[C] = None
    
    def cancel(evenIfRunning: Boolean): Boolean =
      af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)

    def isCancelled(): Boolean =
      af.isCancelled() || bf.isCancelled()
      
    def isDone(): Boolean =
      resultOpt.isDefined
      
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))
      
    def get(): C =
      compute(Long.MaxValue)

    private def compute(timeoutMs:Long):C = {
      resultOpt match {
        case Some(r) => r
        case None =>
          val start = System.currentTimeMillis
          val ar = af.get(timeoutMs, TimeUnit.MILLISECONDS)
          val stop = System.currentTimeMillis
          val at = stop - start
          val br = bf.get(timeoutMs - at, TimeUnit.MILLISECONDS)
          resultOpt = Some(f(ar, br))
          resultOpt.get
      }
    }
  }

  // Exercise 7.4: asyncF.
  def asyncF[A,B](f: A => B): A => Par[B] =
     a => fork(unit(f(a)))
  
  // Exercise 7.5: sequence.
  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    as.foldRight(unit(List[A]()))((h,t) => map2(h, t)(_ :: _))
  
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
   }
    
    
  // Exercise 7.6: parFilter.
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val parBools = parMap(l)(f)
    map2(unit(l), parBools)((elt, bool) => elt.zip(bool).filter(_._2).map(_._1))
  }
  
  // Exercise 7.11: choiceN.
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =     es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }
    
  // Exercise 7.11: choice and terms of choiceN.
  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))
  
  // Exercise 7.13: chooser.
  def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }
    
  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(int => choices(int))
    
  def choiceViaChooser[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    chooser(a)(if (_) ifTrue else ifFalse)
  
  // Exercise 7.14: join.
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())
  
  // Exercise 7.14: joinViaFlatMap.
  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }    
        
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  // Exercise 7.14: flatMapViaJoin.  
  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))
 
  
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

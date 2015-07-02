package fpinscala.parallelism

import org.specs2.mutable._
import org.specs2.specification.AllExpectations
import org.specs2.matcher.DataTables
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.util.concurrent._

@RunWith(classOf[JUnitRunner])
class ParSpec extends Specification with DataTables {
  // Run the tests sequentially.
  sequential

  //val maxNbOfThreadsInTests = 8
  //val es = Executors.newFixedThreadPool(maxNbOfThreadsInTests)
  val es = Executors.newCachedThreadPool()

  // Concrete classes for the generic types A, B and C.
  sealed class ConcreteType(a: String) {}
  case class CA(val a: String) extends ConcreteType(a)
  case class CB(val b: String) extends ConcreteType(b)
  case class CC(val c: String) extends ConcreteType(c)

  case class CallableCA(durationMillis: Int, value: String = "") extends Callable[CA] {
    def call(): CA = {
      Thread.sleep(durationMillis)
      CA(s"${value}${durationMillis}ms")
    }
  }

  case class CallableCB(durationMillis: Int, value: String = "") extends Callable[CB] {
    def call(): CB = {
      Thread.sleep(durationMillis)
      CB(s"${value}${durationMillis}ms")
    }
  }

  "The following exercises should be correct" >> {

    "Exercise 7.3: map2 that respects the contract of timeouts on Future." in {
      "callableA duration" || "callableB duration" || "Wait until" || "exception thrown" |
        50 !! 100 !! 125 !! false | // 125 as the execution time should be roughly under 150ms but above 100ms.   
        100 !! 50 !! 125 !! false |
        150 !! 150 !! 225 !! false | // 125 as the execution time should be roughly under 300ms but above 150ms.
        190 !! 50 !! 180 !! true | // 190 > 190 so to throw an exception. 
        50 !! 190 !! 180 !! true |> {
          (durationA, durationB, waitingDuration, exceptionToBeThrown) =>
            def parA(es: ExecutorService): Future[CA] = es.submit(CallableCA(durationA))
            def parB(es: ExecutorService): Future[CB] = es.submit(CallableCB(durationB))
            //val parC = Par.map2(parA, parB)((a, b) => CC(a + "/" + b))
            val parC = Par.map2WithTimeOut(parA, parB)((a, b) => CC(a + "/" + b))
            try {
              val res = parC(es).get(waitingDuration, TimeUnit.MILLISECONDS)
              if (exceptionToBeThrown)
                ko("A TimeoutException should have been thrown!")
              else
                res must_== CC(s"CA(${durationA}ms)/CB(${durationB}ms)")
            } catch {
              case te: TimeoutException if (exceptionToBeThrown) => ok
              case ex: Exception                                 => ko(ex.getMessage())
            }
        }
    }
    "Exercise 7.4: asyncF." in {
      def f(a: CA) = CB(a.a)
      val par = Par.asyncF(f)
      par(CA("a"))(es).get must_== CB("a")
    }

    "Exercise 7.5: sequence." in {
      val parA = (es: ExecutorService) => es.submit(CallableCA(50, "1-"))
      val parB = (es: ExecutorService) => es.submit(CallableCA(50, "2-"))
      val parC = (es: ExecutorService) => es.submit(CallableCA(50, "3-"))
      val ps = List(parA, parB, parC)
      val parList = Par.sequence(ps)

      try {
        // The execution time must be roughly lower than 100ms.
        val res = parList(es).get(90, TimeUnit.MILLISECONDS)
        res must_== List(CA("1-50ms"), CA("2-50ms"), CA("3-50ms"))
      } catch {
        case ex: Throwable => ko(ex.getMessage())
      }
    }
  }

  "Exercise 7.6: parFilter." in {
    // Note: With a newFixedThreadPool and iIf the list contains more than 5 elements the test hangs! It is due at least to the size
    // of the newFixedThreadPool but I don't get the exact reason.
    val as = List(CA("1"), CA("2"), CA("3"), CA("4"), CA("5"), CA("6"), CA("7"), CA("8"))
    val parList = Par.parFilter(as) { ca =>
      Thread.sleep(50)
      ca.a == "1" || ca.a == "8"
    }

    try {
      // The execution time must be roughly lower than 2x50ms.
      val res = parList(es).get(90, TimeUnit.MILLISECONDS)
      res must_== List(CA("1"), CA("8"))
    } catch {
      case ex: Throwable => ko(ex.getMessage())
    }
  }

  "Exercise 7.11: choiceN." in {
    val callableN = new Callable[Int] {
      def call(): Int = { Thread.sleep(200); 1 }
    }

    val n = (es: ExecutorService) => es.submit(callableN)
    val parA = (es: ExecutorService) => es.submit(CallableCA(5000, "A-"))
    val parB = (es: ExecutorService) => es.submit(CallableCA(50, "B-"))
    val parC = (es: ExecutorService) => es.submit(CallableCA(5000, "C-"))
    val parD = (es: ExecutorService) => es.submit(CallableCA(50, "D-"))

    val t0 = System.currentTimeMillis()
    val parChoiceN = Par.choiceN(n)(List(parA, parB, parC, parD))
    val res = parChoiceN(es).get
    val t1 = System.currentTimeMillis()
    val duration = t1 - t0
    (res must_== CA("B-50ms")) and
      (duration must be_<(1000L)) and // Must be roughly < 5000ms.
      (duration must be_>(225L)) // Must be roughly > 200ms  (callableN + parB).
  }

  "Exercise 7.11: choice and terms of choiceN." in {
    val callableChoice = new Callable[Boolean] {
      def call(): Boolean = {
        Thread.sleep(200)
        true
      }
    }
    val par1 = (es: ExecutorService) => es.submit(CallableCA(5000, "C-"))
    val par2 = (es: ExecutorService) => es.submit(CallableCA(50, "D-"))
    val cond = (es: ExecutorService) => es.submit(callableChoice)

    val t0 = System.currentTimeMillis()
    val parChoice = Par.choiceViaChoiceN(cond)(par2, par1)
    val res = parChoice(es).get
    val t1 = System.currentTimeMillis()
    val duration = t1 - t0
    (res must_== CA("D-50ms")) and
      (duration must be_<(1000L)) and // Must be roughly < 5000ms.
      (duration must be_>(225L)) // Must be roughly > 200ms (callableChoice + par2).
  }

  "Exercise 7.13: chooser." in skipped {
    val callableN = new Callable[Int] {
      def call(): Int = {
        Thread.sleep(10)
        1
      }
    }
    val callableChoice = new Callable[Boolean] {
      def call(): Boolean = {
        Thread.sleep(20)
        true
      }
    }
    val n = (es: ExecutorService) => es.submit(callableN)
    val parChoice = (es: ExecutorService) => es.submit(callableChoice)
    val parA = (es: ExecutorService) => es.submit(CallableCA(5000, "A-"))
    val parB = (es: ExecutorService) => es.submit(CallableCA(50, "B-"))
    val parC = (es: ExecutorService) => es.submit(CallableCA(5000, "C-"))
    val parD = (es: ExecutorService) => es.submit(CallableCA(50, "D-"))

    val t0 = System.currentTimeMillis()
    val res0 = Par.choiceNViaChooser(n)(List(parA, parB, parC, parD))
    val res1 = Par.choiceViaChooser(parChoice)(parD, parC)
    val t1 = System.currentTimeMillis()
    (res0(es).get must_== CA("B-50ms")) and
      (res1(es).get must_== CA("D-50ms")) and
      ((t1 - t0) must be_<(5000L))
  }

  "Exercise 7.14: join." in skipped {
    import Par._
    val callablePar = new Callable[Par[CA]] {
      def call(): Par[CA] = {
        Thread.sleep(10)
        val parA = (es: ExecutorService) => es.submit(CallableCA(10, "A-"))
        parA
      }
    }
    val parPar = (es: ExecutorService) => es.submit(callablePar)
    join(parPar)(es).get must_== CA("A-10ms")
  }

  "Exercise 7.14: joinViaFlatMap." in skipped {
    import Par._
    val callablePar = new Callable[Par[CA]] {
      def call(): Par[CA] = {
        Thread.sleep(10)
        val parA = (es: ExecutorService) => es.submit(CallableCA(10, "A-"))
        parA
      }
    }
    val parPar = (es: ExecutorService) => es.submit(callablePar)
    joinViaFlatMap(parPar)(es).get must_== CA("A-10ms")
  }

  "Exercise 7.14: flatMapViaJoin." in skipped {
    val callableN = new Callable[Int] {
      def call(): Int = {
        Thread.sleep(10)
        1
      }
    }
    val callableChoice = new Callable[Boolean] {
      def call(): Boolean = {
        Thread.sleep(20)
        true
      }
    }
    val n = (es: ExecutorService) => es.submit(callableN)
    val parChoice = (es: ExecutorService) => es.submit(callableChoice)
    val parA = (es: ExecutorService) => es.submit(CallableCA(5000, "A-"))
    val parB = (es: ExecutorService) => es.submit(CallableCA(50, "B-"))
    val parC = (es: ExecutorService) => es.submit(CallableCA(5000, "C-"))
    val parD = (es: ExecutorService) => es.submit(CallableCA(50, "D-"))

    val t0 = System.currentTimeMillis()
    val res = Par.flatMapViaJoin(n)(List(parA, parB, parC, parD))
    val t1 = System.currentTimeMillis()
    (res(es).get must_== CA("B-50ms")) and ((t1 - t0) must be_<(5000L))
  }

}
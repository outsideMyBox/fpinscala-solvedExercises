package fpinscala.datastructures

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class Exercise3Tests {

  @Test 
  def testExercise_3_01() {		// match
	  assertEquals(List.resultOfMatchExpression, 3)
  }

  // ------------------ List ------------------------------------------------------------------------------------------------

  @Test 
  def testExercise_3_02() {		// tail
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.tail(l1), List(2,3,4,5))

	  val l2 = List()
	  try {
		  val t = List.tail(l2)
		  fail("tail of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _ => fail("tail of empty list - test failed")
	  }

	  val l3 = List()
	  try {
		  val t = List.tail(l3)
		  fail("tail of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _ => fail("tail of empty list - test failed")
	  }
  }

  @Test 
  def testExercise_3_03() {		// setHead
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.setHead(l1, 99), List(99, 2,3,4,5))

	  val l2 = List()
	  try {
		  val t = List.tail(l2)
		  fail("setHead of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _ => fail("setHead of empty list - test failed")
	  }
	  
	  val l3 = Nil
	  try {
		  val t = List.tail(l3)
		  fail("setHead of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _ => fail("setHead of empty list - test failed")
	  }
  }

  @Test 
  def testExercise_3_04() {		// drop
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.drop(l1, 0), List(1,2,3,4,5))

  	  val l2 = List(1,2,3,4,5)
	  assertEquals(List.drop(l2, 1), List(2,3,4,5))

  	  val l3 = List(1,2,3,4,5)
	  assertEquals(List.drop(l3, 5), Nil)

  	  val l4 = List(1,2,3,4,5)
	  assertEquals(List.drop(l4, 6), Nil)
	  
  	  val l5 = Nil
	  assertEquals(List.drop(l5, 99), Nil)
  }

  @Test 
  def testExercise_3_05() {		// dropWhile
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.dropWhile(l1, ((x:Int) => x<4)), List(4,5))

  	  val l2 = List(1,2,3,4,5)
	  assertEquals(List.dropWhile(l2, ((x:Int) => x<6)), Nil)

  	  val l3 = List(1,2,3,4,5)
	  assertEquals(List.dropWhile(l3, ((x:Int) => x<1)), List(1,2,3,4,5))

  	  val l4 = Nil
	  assertEquals(List.dropWhile(l4, ((x:Int) => x<99)), Nil)
  }

  @Test 
  def testExercise_3_06() {		// init
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.init(l1), List(1,2,3,4))

	  val l2 = List(1)
	  assertEquals(List.init(l2), Nil)

	  val l3 = Nil
	  try {
		  val t = List.init(l3)
		  fail("init of empty list - test failed")
	  }
	  catch {
	  	case ex: RuntimeException => // expected
	  	case _ => fail("init of empty list - test failed")
	  }
  }

  @Test 
  def testExercise_3_07() {	// just a question, nothing to implement
	  // nothing to test here
  }

  @Test 
  def testExercise_3_08() {	// returns the original list, right?
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.foldRight(l1, Nil: List[Int])(Cons(_,_)), l1)
  }

  @Test 
  def testExercise_3_09() {	// length via foldRight
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.length(l1), 5)
	  
	  val l2 = List(1)
	  assertEquals(List.length(l2), 1)
	  
	  val l3 = Nil
	  assertEquals(List.length(l3), 0)
  }

  @Test 
  def testExercise_3_10() {	// foldLeft
	  val l1 = List(1,2,3,4,5)
	  
	  val l_fr_plus = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_plus = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_plus, l_fl_plus)			// plus is commutative
	  
	  val l_fr_minus = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_minus = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_minus, l_fl_minus)		// minus is not
	  
	  val l_fr_mult = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_mult = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_mult, l_fl_mult)			// multiply is commutative
	  
	  val l_fr_div = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_div = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_div, l_fl_div)			// division is not
  }

  @Test 
  def testExercise_3_11() {		// sum, product and length via foldLeft
  	  val l1 = List(1,2,3,4,5)  
  	  val s = List.sum2(l1)
  	  assertEquals(s, 15)
  	  
  	  val l2 = List(1.0,2.0,3.0,4.0,5.0)
  	  val p = List.product2(l2)
  	  assertEquals(p, 120.0, 0.00001)
  	  
  	  val l3 = List(1,2,3,4,5)  
  	  val l = List.length2(l3)
  	  assertEquals(l, 5)  	  
  }

  @Test 
  def testExercise_3_12() {		// reverse
	  val l1 = List(1,2,3,4,5)
  	  assertEquals(List.reverse(l1), List(5,4,3,2,1))
  	  
	  val l2 = List(1)
  	  assertEquals(List.reverse(l2), l2)
  	  
	  val l3 = Nil
  	  assertEquals(List.reverse(l3), Nil)
  }

  @Test 
  def testExercise_3_13() {		// foldRight, foldLeft
	  val l1 = List(1,2,3,4,5)
	  
	  val l_fr_plus = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_plus = List.foldLeft (l1, 0)((x,y) => x+y)
	  val l_fl_plus_Viafr = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_plus, l_fl_plus)			// plus is commutative
	  assertEquals(l_fl_plus, l_fl_plus_Viafr)
	  
	  val l_fr_minus = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_minus = List.foldLeft (l1, 0)((x,y) => x-y)
	  val l_fl_minus_Viafr = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_minus, l_fl_minus)		// minus is not
	  assertEquals(l_fl_minus, l_fl_minus_Viafr)
	  
	  val l_fr_mult = List.foldRight(l1, 0)((x,y) => x+y)
	  val l_fl_mult = List.foldLeft (l1, 0)((x,y) => x+y)
	  val l_fl_mult_Viafr = List.foldLeft (l1, 0)((x,y) => x+y)
	  assertEquals(l_fr_mult, l_fl_mult)			// multiply is commutative
	  assertEquals(l_fl_mult, l_fl_mult_Viafr)
	  
	  val l_fr_div = List.foldRight(l1, 0)((x,y) => x-y)
	  val l_fl_div = List.foldLeft (l1, 0)((x,y) => x-y)
	  val l_fl_div_Viafr = List.foldLeft (l1, 0)((x,y) => x-y)
	  assertNotEquals(l_fr_div, l_fl_div)			// division is not
	  assertEquals(l_fl_div, l_fl_div_Viafr)
  }

  @Test 
  def testExercise_3_14() {		// append
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.append(l1, List(99)), List(1,2,3,4,5,99))
	  
	  val l2 = List(1)
	  assertEquals(List.append(l2, List(99)), List(1, 99))
	  
	  val l3 = Nil
	  assertEquals(List.append(l3, List(99)), List(99))

  	  val l4 = List(1,2,3,4,5)
	  assertEquals(List.append(l1, Nil), l4)
	  
	  val l5 = List()
	  assertEquals(List.append(l5, Nil), l5)
	  
	  val l6 = Nil
	  assertEquals(List.append(l6, Nil), l6)
  }

  @Test 
  def testExercise_3_15() {		// concat
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.concat(List(l1, List(99))), List(1,2,3,4,5,99))
	  
	  val l2 = List(1)
	  assertEquals(List.concat(List(l2, List(99))), List(1, 99))
	  
	  val l3 = Nil
	  assertEquals(List.concat(List(l3, List(99))), List(99))

	  val l4 = List(1,2,3,4,5)
	  assertEquals(List.concat(List(l1, List(99), Nil)), List(1,2,3,4,5,99))
	  
	  val l5 = List(1)
	  assertEquals(List.concat(List(l2, List(99), List(0))), List(1, 99, 0))
	  
	  val l6 = Nil
	  assertEquals(List.concat(List(l3, List(99), Nil, Nil)), List(99))
  }

  @Test 
  def testExercise_3_16() {		// add one 
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.add1(l1), List(2,3,4,5,6))
	  
	  val l2 = List(1)
	  assertEquals(List.add1(l2), List(2))
	  
	  val l3 = Nil
	  assertEquals(List.add1(l3), Nil)  
  }

  @Test 
  def testExercise_3_17() {		// convert to String
	  val l1 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List.doubleToString(l1), List("1.0","2.0","3.0","4.0","5.0"))
	  
	  val l2 = List(1.0)
	  assertEquals(List.doubleToString(l2), List("1.0"))
	  
	  val l3 = Nil
	  assertEquals(List.doubleToString(l3), Nil)  
  }

  @Test 
  def testExercise_3_18() {		// map
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.map(l1)(x => x+1), List(2,3,4,5,6))
	  
	  val l2 = List(1)
	  assertEquals(List.map(l2)(x => x+1), List(2))
	  
	  val l3 = List[Int]()
	  assertEquals(List.map(l3)(x => x+1), Nil)  
    	  
	  val l4 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List.map(l4)(x => "" + x), List("1.0","2.0","3.0","4.0","5.0"))
	  
	  val l5 = List(1.0)
	  assertEquals(List.map(l5)(x => "" + x), List("1.0"))
	  
	  val l6 = Nil
	  assertEquals(List.map(l6)(x => "" + x), Nil)  
  }

  @Test 
  def testExercise_3_19() {		// filter
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.filter(l1)(x => (x%2)==0), List(2,4))

	  val l2 = List(1)
	  assertEquals(List.filter(l2)(x => x<2), l2)
	  
	  val l3 = List[Int]()
	  assertEquals(List.filter(l3)(x => (x%2)!=0), Nil)  
    	  
	  val l4 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List.filter(l4)(x => (x%2)==0), List(2.0,4.0))
	  
	  val l5 = List(1.0)
	  assertEquals(List.filter(l5)(x => (x%2)==0), Nil)  
	  
	  val l6 = List[Int]()
	  assertEquals(List.filter(l6)(x => (x%2)==0), Nil)  
  }

  @Test 
  def testExercise_3_20() {		// flat map
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.flatMap(l1)(x => List(x,x)), List(1,1,2,2,3,3,4,4,5,5))
	  
	  val l2 = List(1)
	  assertEquals(List.flatMap(l2)(x => List(x+1, x+1)), List(2,2))
	  
	  val l3 = List[Int]()
	  assertEquals(List.flatMap(l3)(x => Nil), Nil)  
  }

  @Test 
  def testExercise_3_21() {		// filter via flat map
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.filterViaFlatMap(l1)(x => (x%2)==0), List(2,4))

	  val l2 = List(1)
	  assertEquals(List.filterViaFlatMap(l2)(x => x<2), l2)
	  
	  val l3 = List[Int]()
	  assertEquals(List.filterViaFlatMap(l3)(x => (x%2)!=0), Nil)  
    	  
	  val l4 = List(1.0,2.0,3.0,4.0,5.0)
	  assertEquals(List.filterViaFlatMap(l4)(x => (x%2)==0), List(2.0,4.0))
	  
	  val l5 = List(1.0)
	  assertEquals(List.filterViaFlatMap(l5)(x => (x%2)==0), Nil)  
	  
	  val l6 = List[Int]()
	  assertEquals(List.filterViaFlatMap(l6)(x => (x%2)==0), Nil)  
  }

  @Test 
  def testExercise_3_22() {		// add pair wise
	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.addPairwise(l1, l1), List(2,4,6,8,10))
  	  assertEquals(List.addPairwise(l1, Nil), Nil)

	  val l2 = List[Int]()
	  assertEquals(List.addPairwise(l2, l2), Nil)
  }

  @Test 
  def testExercise_3_23() {		// zipWith
  	  val l1 = List(1,2,3,4,5)
	  assertEquals(List.zipWith(l1, l1)((x,y) => x+y), List(2,4,6,8,10))
	  assertEquals(List.zipWith(l1, l1)((x,y) => x*y), List(1,4,9,16,25))
  	  assertEquals(List.zipWith(l1, List[Int]())((x,y) => x+y), Nil)
  	  
	  val l2 = List[Int]()
	  assertEquals(List.zipWith(l2, l2)((x,y) => x+y), Nil)
  }

  @Test 
  def testExercise_3_24() {		// has subsequence?
	  val l1 = List(1,2,3,4,5)

	  assertTrue(List.hasSubsequence(l1, l1))
	  assertTrue(List.hasSubsequence(l1, Nil))
	  assertTrue(List.hasSubsequence(l1, List[Int]()))
	  assertFalse(List.hasSubsequence(l1, List(99)))
	  assertFalse(List.hasSubsequence(l1, List(2,3,5,4)))
	  assertFalse(List.hasSubsequence(Nil, List(99)))
  }

  // ------------------ Tree ------------------------------------------------------------------------------------------------
  
  @Test 
  def testExercise_3_25() {		// size
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(Tree.size(t1), 7)

	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(Tree.size(t2), 1)
  }

  @Test 
  def testExercise_3_26() {		// maximum
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(Tree.maximum(t1), 8)

	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(Tree.maximum(t2), 99)
  }

  @Test 
  def testExercise_3_27() {		// depth
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Branch(Leaf[Int](8), Leaf[Int](9))))	  
	  assertEquals(Tree.depth(t1), 3)

	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(Tree.depth(t2), 0)
  }

  @Test 
  def testExercise_3_28() {		// map
	  val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Branch(Leaf[Int](8), Leaf[Int](9))))	  
	  val mappedt1 = Tree.map(t1)((x: Int) => ""+x)
	  assertEquals(mappedt1, Branch (Branch(Leaf[String]("5"), Leaf[String]("6")), Branch(Leaf[String]("7"), Branch(Leaf[String]("8"), Leaf[String]("9")))))

	  val t2: Tree[Int] = Leaf(99)
	  val mappedt2 = Tree.map(t2)((x: Int) => ""+x)
	  assertEquals(mappedt2, Leaf("99"))
  }

  @Test 
  def testExercise_3_29() {		// fold and size/maximum/depth via fold
	  def sizeViaFold[A](t: Tree[A]):   Int = Tree.fold(t)(a => 1)(1 + _ + _)
    	
      val t1: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(Tree.size(t1), 7)
	  assertEquals(Tree.size(t1), sizeViaFold(t1))
	  val t2: Tree[Int] = Leaf(99)
	  assertEquals(Tree.size(t2), 1)
	  assertEquals(Tree.size(t2), sizeViaFold(t2))  

  	  def maximumViaFold(t: Tree[Int]): Int = Tree.fold(t)(a => a)(_ max _)
  	  
	  val t3: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Leaf[Int](8)))
	  assertEquals(Tree.maximum(t3), 8)
	  assertEquals(Tree.maximum(t3), maximumViaFold(t3))
	  val t4: Tree[Int] = Leaf(99)
	  assertEquals(Tree.maximum(t4), 99)
	  assertEquals(Tree.maximum(t4), maximumViaFold(t4))
	  
  	  def depthViaFold[A](t: Tree[A]):  Int = Tree.fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
  	  
	  val t5: Tree[Int] = Branch (Branch(Leaf[Int](5), Leaf[Int](6)), Branch(Leaf[Int](7), Branch(Leaf[Int](8), Leaf[Int](9))))	  
	  assertEquals(Tree.depth(t5), 3)
	  assertEquals(Tree.depth(t5), depthViaFold(t5))
	  val t6: Tree[Int] = Leaf(99)
	  assertEquals(Tree.depth(t6), 0)
	  assertEquals(Tree.depth(t6), depthViaFold(t6))
  }
}

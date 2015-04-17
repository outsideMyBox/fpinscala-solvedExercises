package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25: size.
  def size[A](t: Tree[A]): Int = ???

  // Exercise 3.26: maximum.
  def maximum(t: Tree[Int]): Int = ???

  // Exercise 3.27: depth.
  def depth[A](t: Tree[A]): Int = ???

  // Exercise 3.28: map.
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = ???

  // Exercise 3.29: size, maximum, depth and map with fold.
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = ???

  def sizeViaFold[A](t: Tree[A]): Int = ???

  def maximumViaFold(t: Tree[Int]): Int = ???
  
  def depthViaFold[A](t: Tree[A]): Int = ???
  
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = ???

}
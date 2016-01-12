package fpinscala.errorhandling

import scala.{ Option => _, Some => _, Either => _, _ } // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  //Exercise 1
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None    => None
  }

  //Exercise 1
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  //Exercise 1
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None    => default
  }

  //Exercise 1
  def orElse[B >: A](default: => B): Option[B] = Some(getOrElse(default))

  //Exercise 1
  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) this else None)
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

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = Some(xs.sum / xs.length)
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //Exercise 3
  //Initially I did this with pattern matching but I've grown more comfortable with the idea of chaining flatmaps
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))

  //Exercise 4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]]): List[A] = a match {
      case Some(x) :: Nil => List(x)
      case Some(x) :: xs  => Some(x).map(y => x :: go(xs)).getOrElse(Nil)
      case _              => Nil
    }
    val newList = go(a)
    if (newList.size == a.size) Some(newList) else None
  }

  //Exercise 4, another way to do it
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil     => Some(Nil)
      case x :: xs => x flatMap (y => sequence_1(xs) map (z => y :: z))
    }

  //Exercise 5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil     => Some(Nil)
    case x :: xs => f(x) flatMap (y => traverse(xs)(f) map (z => y :: z))
  }
  
  //Exercise 5, sequence in terms of traverse
  def sequenceInTermsOfTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
  
  def tryThis[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }
}

object OptionExercises extends App {
  import fpinscala.errorhandling.Option._
  val exercise1 = {
    val opt1 = Some(1)
    val opt2: Option[Int] = None
    println(s"Map 1 to some ${opt1.map(_ + 1)}")
    println(s"Map 1 to none ${opt2.map(_ + 1)}")
    println(s"Flatmap Some(1) ${opt1.flatMap(x => Some(x + 1))}")
    println(s"Flatmap None ${opt2.flatMap(x => Some(x + 1))}")
    println(s"Get or else 2 Some(1) ${opt1.getOrElse(2)}")
    println(s"Get or else 2 None ${opt2.getOrElse(2)}")
    println(s"Or else Some(2) Some(1) ${opt1.orElse(2)}")
    println(s"Or else Some(2) None ${opt2.orElse(2)}")
    println(s"Filter when ne 1 Some(1) ${opt1.filter(_ != 1)}")
    println(s"Filter when eq 1 Some(1) ${opt1.filter(_ == 1)}")
    println(s"Filter when ne 1 None ${opt2.filter(_ != 1)}")
  }

  val exercise2 = {
    //Exercise 2
    val seq1: Seq[Double] = Seq(1, 2, 3, 4, 5)
    println(s"Variance of ${variance(seq1)}")
  }

  val exercise3 = {
    val opt1 = Some(2)
    val opt2 = Some(5)
    val opt3 = map2(opt1, opt2)((x, y) => x * y)
    println(s"map2 of opt3 is $opt3")
  }

  val exercise4 = {
    val list1 = List(Some(1), Some(2), Some(3))
    println(s"Sequence is ${sequence(list1)}")

    val list2 = List(Some(1), None, Some(3), None)
    println(s"Sequence is ${sequence(list2)}")
  }

  val exercise5 = {
    val list1 = List("1", "2", "3")
    println(s"Traversal is ${traverse(list1)(x => tryThis { x.toInt })}")

    val list2 = List("1", "2", "Snikes")
    println(s"Traversal is ${traverse(list2)(x => tryThis { x.toInt })}")

    val list3 = List(Some(1), Some(2), Some(3))
    println(s"Sequence2 is ${sequenceInTermsOfTraverse(list3)}")

    val list4 = List(Some(1), None, Some(3), None)
    println(s"Sequence2 is ${sequenceInTermsOfTraverse(list4)}")
  }

}
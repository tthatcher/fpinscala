package fpinscala.errorhandling

import scala.{ Option => _, Either => _, Left => _, Right => _, _ } // hide std library `Option` and `Either`, since we are writing our own in this chapter

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  //Exercise 6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x)   => Right(f(x))
    case x: Left[E] => x
  }

  //Exercise 6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case x: Right[B] => x
    case _           => b
  }

  //Exercise 6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x)   => f(x)
    case x: Left[E] => x
  }

  //Exercise 6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(x => b.map(y => f(x, y)))
}

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  //Issue with currying I think, if this wasn't curried it would be ok and I could overload
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case x :: xs => f(x).flatMap(y => traverse(xs)(f) map (z => y :: z))
    case Nil     => Right(Nil)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object EitherExercises extends App {
import fpinscala.errorhandling.Either._
  val exercise6 = {
    val l: Either[String, Int] = Left("Bad value")
    val r: Either[String, Int] = Right(5)
    println(s"Map right ${r.map(_ + 5)}")
    println(s"Map left ${l.map(_ + 5)}")
    println(s"Or else 10 right ${r.orElse(Right(10))}")
    println(s"Or else 10 left ${l.orElse(Right(10))}")
    println(s"Flatmap right ${r.flatMap(x => Right(x + 5))}")
    println(s"Flatmap left ${l.flatMap(x => Right(x + 5))}")
    println(s"Map2 right ${r.map2(Right(10))(_ + _)}")
    println(s"Map2 right with left input ${r.map2(Left("Bad value again"): Either[String, Int])(_ + _)}")
    println(s"Map2 left ${l.map2(Right(10))(_ + _)}")
  }

  val exercise7 = {
    val optList1 = List(Right(1), Right(50), Right(30))
    val optList2 = List(Right(1), Left("This is bad input"), Right(30))
    println(s"Sequence with good input ${sequence(optList1)}")
    println(s"Sequence with bad input ${sequence(optList2)}")
    val list1 = List(1, 2, 3, 5, -10)
    val list2 = List(1, 2, 3, 5, 10)
    def gt0(x: Int) = if (x > 0) Right(x) else Left("Bad input")
    println(s"Traverse with bad input ${traverse(list1)(gt0)}")
    println(s"Traverse with good input ${traverse(list2)(gt0)}")
  }

  //For 4.8 We'd need to accumulate Errors in a list instead of just taking one, and modify the 
  // functions that operate on either to add errors to that sequence

}
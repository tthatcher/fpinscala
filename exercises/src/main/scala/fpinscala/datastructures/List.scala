package fpinscala.datastructures

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
    case Nil                                   => 42
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

  //Exercise 2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
    case Nil         => throw new IllegalArgumentException("Can't get tail of nil list")
  }

  def head[A](l: List[A]): A = l match {
    case Cons(x, xs) => x
    case Nil         => throw new IllegalArgumentException("Can't get head of nil list")
  }

  //Exercise 3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(x, xs) => Cons(h, xs)
    case Nil         => throw new IllegalArgumentException("Can't set head of nil list")
  }

  //Some other attempt I did
  def dropTailRecursiveDecomp[A](list: List[A], n: Int): List[A] = {
    if (list == Nil) {
      throw new IllegalArgumentException("Can't drop elements from nil list")
    }
    @annotation.tailrec
    def go(l: List[A], i: Int): List[A] = {
      if (i == n) {
        l
      } else {
        go(tail(l), i + 1)
      }
    }
    go(list, 0)
  }

  //Exercise 4
  def drop[A](l: List[A], n: Int): List[A] = {
    def go(l: List[A], i: Int): List[A] = l match {
      case Cons(x, xs) => if (n != i) go(xs, i + 1) else l
      case Nil         => throw new IllegalArgumentException("Can't drop elements from nil list")
    }
    go(l, 0)
  }

  //Some other attempt I did for exercise 5
  def dropWhileTailRecursive[A](l: List[A], f: A => Boolean): List[A] = {
    if (l == Nil) {
      throw new IllegalArgumentException("Can't drop elements from nil list")
    }
    @annotation.tailrec
    def go(l: List[A]): List[A] = {
      if (l == Nil) {
        Nil
      } else if (f(List.head(l))) {
        go(List.tail(l))
      } else {
        l
      }
    }
    go(l)
  }

  //Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    case Nil         => l
  }

  //Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
    case _            => Nil
  }

  //Exercise 9
  def lengthFoldRight[A](as: List[A]): Int = {
    foldRight[A, Int](as, 0)((x, y) => y + 1)
  }

  //Exercise 10. Note tailrec annotation.
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case Nil         => z
  }

  //Exercise 11
  def sumfoldLeft(as: List[Int]): Int = {
    foldLeft[Int, Int](as, 0)(_ + _)
  }

  //Exercise 11
  def product(as: List[Int]): Int = {
    foldLeft[Int, Int](as, 1)(_ * _)
  }

  //Exercise 11
  def length[A](as: List[A]): Int = {
    foldLeft[A, Int](as, 0)((x, y) => x + 1)
  }

  //Exercise 12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))
  }

  def appendFoldRight[A](as: List[A], r: List[A]): List[A] = {
    foldRight(as, r)((x, y) => Cons(x, y))
  }

  //The one from the answer key is better.
  //Exercise 15
  def flattenMine[A](as: List[List[A]]): List[A] = {

    def flattenHelper[A](ls: List[A], as: List[List[A]]): List[A] = {
      ls match {
        case Cons(x, xs) => Cons(x, Cons(List.head(xs), flattenHelper(List.tail(xs), as)))
        case Nil         => flattenMine(as)
      }
    }
    as match {
      case Cons(x, xs) => Cons(List.head(x), flattenHelper(List.tail(x), xs))
      case Nil         => Nil
    }
  }

  //From answers. 
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  //Exercise 16
  def addOne(l: List[Int]): List[Int] = l match {
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
    case Nil         => Nil
  }

  //Exercise 17
  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  //Exercise 18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  //Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  //Exercise 20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  //Exercise 21
  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)

  //Exercise 22
  def addLists(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
    case (Nil, _)                   => Nil
    case (_, Nil)                   => Nil
  }

  //Exercise 23
  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case (Nil, _)                   => Nil
    case (_, Nil)                   => Nil
  }

  //Exercise 24
  def hasSubsequence[A](l: List[A], r: List[A]): Boolean = (l, r) match {
    case (Nil, _)                   => false
    case (Cons(x, xs), Cons(y, ys)) => if (x == y) true else hasSubsequence(l, ys)
    case (Cons(x, xs), Nil)         => hasSubsequence(xs, r)
  }
}

object ListExercises extends App {

  //Exercise 1
  //3

  //I'm grouping each exercise into a closure

  //I implemented many of my list functions in such a way where the nil case throws an exception. Probably not ideal.
  val exercise2 = {
    val list: List[Int] = List(1, 2, 3)
    println(s"Tail of list ${List.tail(list)}")
    try {
      List.tail(Nil)
    } catch {
      case ex: Exception => println(s"Nil list throws exception ${ex}")
    }
  }

  val exercise3 = {
    val list: List[Int] = List(1, 2, 3)
    println(s"Set head to 100 ${List.setHead(list, 100)}")
    try {
      List.setHead(Nil, 100)
    } catch {
      case ex: Exception => println(s"Nil list throws exception ${ex}")
    }
  }
  val exercise4 = {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println("Drop 3 elements ${List.drop(list, 3)}")
    try {
      List.drop(Nil, 100)
    } catch {
      case ex: Exception => println(s"Nil list throws exception ${ex}")
    }
  }

  val exercise5 = {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    def not3(x: Int) = x != 3
    println(s"Drop while not 3 ${List.dropWhile(list, not3)}")
    println(s"Drop while not 10 ${List.dropWhile(list, (x: Int) => x != 10)}")
    println(s"Drop with nil input ${List.dropWhile(Nil, not3)}")
  }

  val exercise6 = {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println(s"All but last of list ${List.init(list)}")
    println(s"All but last of nil list ${List.init(Nil)}")
  }

  //Exercise 7
  //Answer: No. Fold right always evaluates the entire list. You can't short circuit it as is.

  val exercise8 = {
    println(s"Same list ${List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))}")
  }

  val exercise9 = {
    val list = List(1, 2, 3, 4, 5, 6)
    println(s"Length: ${List.lengthFoldRight(list)}")
  }

  val exercise10 = {
    //Fold right is not tail recursive and will explode. I'm not going to do this in code.
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println(s"Fold left add ${List.foldLeft(list, 2)(_ + _)}")
  }

  val exercise11 = {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println(s"Sum fold left: ${List.sum(list)}")
    println(s"Product fold left: ${List.product(list)}")
    println(s"Length fold left: ${List.length(list)}")
  }

  val exercise12 = {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println(s"Not reversed: ${list}")
    println(s"Reversed: ${List.reverse(list)}")
  }

  //Exercise 13
  //Answer: Reverse the list

  val exercise14 = {
    val list: List[Int] = List(1, 2, 3, 4, 5)
    println(s"Append using fold right: ${List.appendFoldRight(list, List(2))}")
  }

  val exercise15 = {
    val list = List(List(1, 2, 3), List(4, 5, 6))
    println(s"Flatten: ${List.flattenMine(list)}")
  }

  val exercise16 = {
    val list = List(1, 2, 3, 4)
    println(s"Add one: ${List.addOne(list)}")
  }

  val exercise17 = {
    val list: List[Double] = List(1, 2, 3, 4)
    println(s"Dbl to string: ${List.doubleToString(list)}")
  }

  val exercise18 = {
    val list = List(1, 2, 3, 4)
    println(s"Map plus 5: ${List.map(list)(_ + 5)}")
  }

  val exercise19 = {
    val list = List(1, 2, 3, 4, 4, 1)
    println(s"Filtering on 4: ${List.filter(list)(_ == 4)}")
  }

  val exercise20 = {
    val list = List(1, 2, 3)
    println(s"Flatmap map to List(x,x) : ${List.flatMap(list)(i => List(i, i))}")
  }

  val exercise21 = {
    val list = List(1, 2, 3, 4, 4, 1)
    println(s"Filtering using flatmap on 1: ${List.filterFlatMap(list)(_ == 1)}")
  }

  val exercise22 = {
    val list1 = List(1, 2, 3, 4, 4, 1)
    val list2 = List(1, 2, 3, 4, 4, 99)
    println(s"Adding lists ${List.addLists(list1, list2)}")
  }

  val exercise23 = {
    val list1 = List(1, 2, 3, 4, 4, 1)
    val list2 = List(1, 2, 3, 4, 4, 99)
    println(s"Subtracting lists using zipwith ${List.zipWith(list1, list2)(_ - _)}")
  }

  val exercise24 = {
    val list1 = List(1, 2, 3, 4, 4, 1)
    val list2 = List(1, 2, 3, 4, 4, 99)
    val list3 = List(89, 20, 33)
    println(s"Has subsequence ${List.hasSubsequence(list1, list2)}")
    println(s"Has subsequence ${List.hasSubsequence(list1, list3)}")
  }
}

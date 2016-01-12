package fpinscala.laziness

import Stream._
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

  //Exercise 1
  def toList: List[A] = {
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => h() :: go(t())
      case Empty      => Nil
    }
    go(this)
  }

  //Exercise 2
  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], i: Int = 0): Stream[A] = s match {
      case Cons(h, t) => if (i < n - 1) cons(h(), go(t(), i + 1)) else cons(h(), Empty)
      case _          => Empty
    }
    if (n == 0) Empty else go(this)
  }

  //Exercise 2
  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], i: Int = 0): Stream[A] = s match {
      case Cons(h, t) => if (i < n) go(t(), i + 1) else s
      case _          => Empty
    }
    if (n == 0) this else go(this)
  }

  //Exercise 3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
    case _          => Empty
  }

  //Exercise 4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //Exercise 5
  def takeWhile_1(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  //Exercise 6
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  //Exercise 6
  def headOption_1: Option[A] = foldRight(None: Option[A])((a, b) => a match { case x => Some(x) })

  //Exercise 7
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  //Exercise 7
  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  //Exercise 7
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  //Exercise 7
  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  //Exercise 13
  def mapUnfold[B](f: A => B): Stream[B] = unfold(this)(x => x match {
    case Cons(h, t) => Some((f(h()), t()))
    case _          => None
  })

  //Exercise 13
  def takeUnfold(n: Int): Stream[A] = unfold((this, n))(x => x match {
    case (_, 0)          => None
    case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
    case _               => None
  })

  //Exercise 13
  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this)(x => x match {
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    case _          => None
  })

  //Exercise 13
  //Mine is different than the text but the idea is the same.
  def zipWith[B >: A](r: Stream[B])(f: (A, B) => B): Stream[B] = unfold((this, r))(x => x match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case (_, _)                       => None
  })

  //From github
  def zipWith_github[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _                            => None
  }

  //Exercise 13
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2))(x => x match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
    case (_, _)                       => None
  })

  //Exercise 14
  def startsWith[A](s: Stream[A]): Boolean = {
    val st = unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(h1() == h2(), if (h1() == h2()) t1() -> t2() else Empty -> Empty)
      case (Empty, Empty)               => None
      case (Empty, _)                   => Some(false, Empty -> Empty)
      case (_, _)                       => None
    }
    !st.exists(_ == false)
  }

  //Exercise 15
  //Github way
  def tails: Stream[Stream[A]] = unfold(this)(x => x match {
    case s: Cons[A] => Some(s, s.drop(1))
    case _          => None
  }).append(Empty)

  //Exercise 15
  //My way is worse, didn't remember drop
  def tails_1: Stream[Stream[A]] = unfold(this, Empty: Stream[A])(x => x match {
    case (Cons(h, t), s) => Some(s.append(cons(h(), Empty)), t() -> s.append(cons(h(), Empty)))
    case _               => None
  }).append(Empty)

  //Exercise 16
  //Lazy way
  def scanRightMine[B](z: => B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f)).append(cons(z, Empty))

  //Better way
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, t) => t match {
      case (z, s: Cons[B]) => {
        lazy val h = f(a, z)
        (h, cons(h, s))
      }
      case (z, Empty) => (f(a, z), Empty)
    })._2
  }

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

  //Exercise 8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  //Exercise 8
  //Apparently this is more efficient
  def constant_1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }
  //Exercise 9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //Exercise 10
  def fib(lastTerm: Int = 0, nextTerm: Int = 1): Stream[Int] = cons(lastTerm, fib(nextTerm, lastTerm + nextTerm))

  //Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => Empty
  }

  //Exercise 12
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(a, a))

  //Exercise 12
  def onesUnfold: Stream[Int] = constantUnfold(1)

  //Exercise 12
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  //Exercise 12
  def fibUnfold: Stream[Int] = unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
}

object StreamExercises extends App {
  val exercise1 = {
    val s = Stream(1, 1, 1)
    println(s"To list ${s.toList}")
  }

  val exercise2 = {
    val s = Stream(1, 2, 4, 5)
    println(s"To drop 2 to list ${s.drop(2).toList}")
    println(s"To take 3 to list ${s.take(3).toList}")
  }

  val exercise3 = {
    val s = cons({ println("THIS IS EXPENSIVE STOP"); 1 }, Stream(1, 2, 44))
    println(s"Take while not 44 to list ${s.takeWhile(_ != 44).toList}")
    println(s"Take while not 1 to list ${s.takeWhile(_ != 1).toList}")
    println(s"Take while not 45 to list ${s.takeWhile(_ != 45).toList}")
  }

  val exercise4 = {
    val s = Stream(100, 2, 3, 4, 100)
    println(s"For all not 100 ${s.forAll(_ != 100)}")
    println(s"For all not 200 ${s.forAll(_ != 200)}")
  }

  val exercise5 = {
    val s = cons({ println("THIS IS EXPENSIVE STOP"); 1 }, Stream(1, 2, 44))
    println(s"Take while not 44 to list ${s.takeWhile_1(_ != 44).toList}")
    println(s"Take while not 1 to list ${s.takeWhile_1(_ != 1).toList}")
    println(s"Take while not 45 to list ${s.takeWhile_1(_ != 45).toList}")
  }

  val exercise6 = {
    val s1 = Stream(1, 2, 3, 4, 5)
    val sEmpty = Stream()
    println(s"Stream headoption ${s1.headOption}")
    println(s"Empty Stream headoption ${sEmpty.headOption}")
    println(s"Stream headoption_1 ${s1.headOption_1}")
    println(s"Empty Stream headoption_1 ${sEmpty.headOption_1}")
  }

  val exercise7 = {
    val s1 = Stream("Sally", "Bobby")
    val s2 = Stream("Harry", "Johnny")
    println(s"Reverse mapping ${s1.map(_.reverse.toLowerCase).toList}")
    println(s"Append ${s1.append(s2).toList}")
    println(s"Flatmap ${s1.flatMap(x => Stream(x, x.reverse)).toList}")
    println(s"Filter ${s1.filter(_ != "Bobby").filter(_ != "Bobby").toList}")
  }
  val exercise8 = {
    val twos = Stream.constant_1(2)
    println(s"Take 5 2s ${twos.take(5).toList}")
  }

  val exercise9 = {
    val ns = Stream.from(1)
    println(s"Take 5 ns ${ns.take(5).toList}")
  }

  val exercise10 = {
    val fib = Stream.fib()
    println(s"Take 10 fib ${fib.take(10).toList}")
  }

  val exercise11 = {
    val s = unfold(0)(x => if (x > 10) None else Some("Default", x + 1))
    println(s"Take 100 (only should return 10) ${s.take(100).toList}")
  }

  val exercise12 = {
    val ones = Stream.onesUnfold
    println(s"Take 5 1s ${ones.map(_ + 1).take(5).toList}")
    val twos = Stream.constantUnfold(2)
    println(s"Take 5 2s ${twos.take(5).toList}")
    val ns = Stream.fromUnfold(1)
    println(s"Take 5 ns ${ns.take(5).toList}")
    val fib = Stream.fibUnfold
    println(s"Take 10 fib ${fib.take(10).toList}")
  }

  val exercise13 = {
    val s1 = Stream(1, 2, 3, 4, 5, 88, 100)
    val s2 = Stream(55, 3)
    val s3 = Stream("John", "Long", "Silver")
    println(s"Take 5 map 100 ${s1.mapUnfold(_ + 100).take(5).toList}")
    println(s"Take 5 ${s1.takeUnfold(5).toList}")
    println(s"TakeWhile 5 ${s1.takeWhileUnfold(_ != 88).toList}")
    println(s"ZipWith s2 ${s1.zipWith(s2)(_ - _).toList}")
    println(s"ZipWith s1 ${s1.zipWith(s1)(_ - _).toList}")
    println(s"ZipAll s1 s3 ${s1.zipAll(s3).toList}")
    println(s"ZipAll s1 empty ${s1.zipAll(Stream()).toList}")
  }

  val exercise14 = {
    val s1 = Stream(1, 2, 3, 4, 5, 88, 100)
    val s2 = Stream(1, 2)
    val s3 = Stream(1, 2, 5)
    println(s"Starts with empty ${s1.startsWith(Stream[Int]())}")
    println(s"Starts with s1 ${s1.startsWith(s1)}")
    println(s"Starts with s2 ${s1.startsWith(s2)}")
    println(s"Starts with s3 ${s1.startsWith(s3)}")
    println(s"Starts with s2 starts with s1 ${s2.startsWith(s1)}")
  }

  val exercise15 = {
    val s1 = Stream(1, 2, 3, 4, 5, 88, 100)
    println(s"Tails ${s1.tails.toList.map(_.toList)}")
  }

  val exercise16 = {
    /*Scan right by definition, scans from the right to the left. We need a function that goes 
    right to left. FoldRight works*/
    val s = Stream(1, 2, 3)
    println(s"Scan right ${s.scanRight(0)(_ + _).toList}")
  }

}
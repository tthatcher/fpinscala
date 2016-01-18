package fpinscala.parallelism

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions
import java.util.concurrent.Executors

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil    => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    //Exercise 11
    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit) {
        p(es) { b =>
          eval(es) { ps(b)(es)(cb) }
        }
      }
    }

    //Exercise 11
    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = choiceN(a.map(if (_) 0 else 1))(List(ifTrue, ifFalse))

    //Exercise 12
    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] = es => new Future[V] {
      def apply(cb: V => Unit) {
        p(es) { k =>
          eval(es) { ps(k)(es)(cb) }
        }
      }
    }

    //Exercise 13    
    //Flatmap/Bind
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] = es => new Future[B] {
      def apply(cb: B => Unit) {
        p(es) { a => f(a)(es)(cb) }
      }
    }

    //Exercise 13
    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = chooser(p)(f)

    //Exercise 13
    def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] = chooser(p)(if (_) f else t)

    //Exercise 13
    def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(p)(choices(_))

    //Exercise 14
    def join[A](p: Par[Par[A]]): Par[A] = es => new Future[A] {
      def apply(cb: A => Unit) {
        p(es) { a => a(es)(cb) }
      }
    }

    //Exercise 14
    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

    //Exercise 14
    //Won't work without mapping obviously
    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = join(p.map(f))

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
      def chooser[B](f: A => Par[B]) = Par.chooser(p)(f)
      def flatMap[B](f: A => Par[B]) = chooser(f)
    }
  }
}

//I'm doing it nonblocking because someone was nice enough to create all of the signatures for me here.
//These tests are just to show the algebra works.
object NonblockingExercises extends App {
  import Nonblocking._
  import Nonblocking.Par._

  val executor = Executors.newFixedThreadPool(1)
  def parseInt(s: String) = s.toInt

  val exercise11 = {
    val list = List(unit(parseInt("1")), unit(parseInt("3")))
    val chosenPar1 = choiceN(unit(1))(list)
    println(s"choiceN Will choose 3 -> ${run(executor)(chosenPar1)}")
    val chosenPar2 = choiceViaChoiceN(unit(true))(unit(1), unit(3))
    println(s"choiceViaChoiceN Will choose 1 -> ${run(executor)(chosenPar2)}")
  }

  val exercise12 = {
    val map: Map[Int, Par[String]] = Map(1 -> unit("One"), 2 -> unit("Two"))
    val chosen = choiceMap(unit(2))(map)
    println(s"choiceMap Will choose 'Two' -> ${run(executor)(chosen)}")
  }

  val exercise13 = {
    val chosenPar1 = choiceViaChooser(unit(false))(unit(1), unit(3))
    println(s"choiceViaChooser Will choose 3 -> ${run(executor)(chosenPar1)}")
    val chosenPar2 = choiceNChooser(unit(2))(List(unit(1), unit(3), unit(4)))
    println(s"choiceNChooser Will choose 4 -> ${run(executor)(chosenPar2)}")
  }
  
  val exercise14 = {
    val joinOfOne = join(unit(unit(1)))
    println(s"join will return 1 -> ${run(executor)(joinOfOne)}")
    val flatMapToString = flatMapViaJoin(unit(1))(x => unit(2))
    println(s"flatMapViaJoin will return 2 -> ${run(executor)(flatMapToString)}")
    val joinViaFlatMapOfTwo = joinViaFlatMap(unit(unit(3)))
     println(s"joinViaFlatMap will return 3 -> ${run(executor)(joinViaFlatMapOfTwo)}")
  }

  executor.shutdown()
}


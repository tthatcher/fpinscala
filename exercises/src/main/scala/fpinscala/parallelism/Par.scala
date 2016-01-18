package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

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

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  //Exercise 4
  def asyncF[A, B](f: A => B): A => Par[B] = a => Par.lazyUnit(f(a))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  //Exercise 3
  private case class ComposeFuture[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {
    def get = f(af.get, bf.get)
    def get(timeout: Long, u: TimeUnit) = {
      val t0 = System.nanoTime
      val ar = af.get(timeout, u)
      val t1 = System.nanoTime
      val ab = bf.get(timeout - (t1 - t0), TimeUnit.NANOSECONDS)
      f(ar, ab)
    }
    //Ignoring these....
    def isDone = true
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  //Exercise 3
  def map2RespectTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    ComposeFuture(af, bf, f)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //Exercise 5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case p1 :: p2 :: ps => {
      val p1p2 = map2(p1, p2)((x, y) => List(x, y))
      map2(p1p2, sequence(ps))((x, y) => x ++ y)
    }
    //Not sure what to do with this
    case _ => ExecutorService => UnitFuture(Nil)
  }

  //Exercise 6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    def filter(a: A): Option[A] = if (f(a)) Some(a) else None
    val optList = parMap(as)(filter)
    map(optList)(_.flatten)
  }

  class ParOps[A](p: Par[A]) {

  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

object ParExercises {
  import Par._

  val exercise1 = {
    /*    Something like this
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = Par.unit(f(Par.get(a), Par.get(b)))
    */
  }

  val exercise2 = {
    /*something like type Par[A] = () => A
      we'd need to substitute whatever mechanism we are using for threading in
      for () but I don't know what that would look like.*/
  }

  val exercise6 = {
    val list = List(1, 2, 3, 4, 5)
    println(Par.parFilter(list)(_ != 5))
  }

  val exercise7 = {
    /*  
 *  We know map(unit(x))(f) == unit(f(x))
 *  so map(x)(f) == f(x)
 *  We want map(y)(f compose g) == map(map(y)(g))(f)
 *  
 *  map(map(y)(g))(f) == map(g(y))(f)
 *  map(g(y))(f) == f(g(y))
 *  map(y)(f compose g) == map(y)(f(g))
 *  map(y)(f(g)) == f(g(y))
 *    
 *  therefore  map(y)(f compose g) == map(map(y)(g))(f)
 *  So they are equivalent
  */
  }

  val exercise8 = {
    /*    Just looking through it seems pretty clear that our assumption x == fork(x) won't be true 
    because the executorservice is stateful. It can throw exceptions or not process something based off 
    of the number of threads allocated. We can't guarantee x==fork(x) will always be the result when we call fork*/
  }

  val exercise9 = {
    //Something like this, should need 2N threads to work consistently (? not positive)
    //val ex = Executors.newFixedThreadPool(N)
    //val list = List(1,2,3...N).map(Par.asyncF(x=>x)).map(_.get(ex))
  }

}

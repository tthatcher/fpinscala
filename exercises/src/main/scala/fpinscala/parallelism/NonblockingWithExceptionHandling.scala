package fpinscala.parallelism

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions
import scala.Either
import java.util.concurrent.Executors

object NonblockingWithExceptionHandling {

  trait Future[+A] {
    private[parallelism] def apply(k: Either[Exception, A] => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): Either[Exception, A] = {
      val ref = new java.util.concurrent.atomic.AtomicReference[Either[Exception, A]] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a :Either[Exception, A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Either[Exception, A] => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Either[Exception, A] => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })
  }
}

//Exercise 10

object NonblockingWithExceptionHandlingExercises extends App {
  import NonblockingWithExceptionHandling._

  val exercise10 = {
    val executor = Executors.newFixedThreadPool(1)
    def parseInt(s: String) = {
      try {
        Right(s.toInt)
      } catch {
        case e: Exception => Left(new Exception("It's broken"))
      }
    }
    println(Par.run(executor)(Par.fork(Par.unit(parseInt("notaint")))))
    executor.shutdown()
  }
}

//This explodes silently and waits forever
object NonBlockingFailing extends App {
  import Nonblocking._
  val exercise10 = {
    val executor = Executors.newFixedThreadPool(1)
    def parseInt(s: String) = s.toInt
    println(Par.run(executor)(Par.fork(Par.unit(parseInt("Stuff")))))
    executor.shutdown()
  }
}

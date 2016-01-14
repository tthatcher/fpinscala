package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //Exercise 1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n == Int.MinValue) {
      nonNegativeInt(nextRNG)
    } else {
      (n.abs, nextRNG)
    }
  }

  //Exercise 2
  def doubleNoMap(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val d = (n: Double) / (Int.MaxValue: Double)
    (d, nextRNG)
  }

  //Exercise 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, intRNG) = rng.nextInt
    val (d, doubleRNG) = double(intRNG)
    ((n, d), doubleRNG)
  }

  //Exercise 3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, doubleRNG) = double(rng)
    val (n, intRNG) = doubleRNG.nextInt
    ((d, n), intRNG)
  }

  //Exercise 3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, doubleRNG1) = double(rng)
    val (d2, doubleRNG2) = double(doubleRNG1)
    val (d3, doubleRNG3) = double(doubleRNG2)
    ((d1, d2, d3), doubleRNG3)
  }

  //Exercise 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, xs: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n > 0) {
        val (randInt, nextRNG) = rng.nextInt
        go(n - 1, randInt :: xs, nextRNG)
      } else {
        (xs, rng)
      }
    }
    go(count, Nil, rng)
  }

  //Exercise 4
  //More functional approach, this order is probably more appropriate
  def intsBetter(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val randomList = (1 until count).scanLeft(rng.nextInt)((x, _) => x._2.nextInt)
      (randomList.map(_._1).toList, randomList.last._2)
    }
  }

  //Exercise 5
  def double: Rand[Double] = map(_.nextInt)(x => (x: Double) / (Int.MaxValue: Double))

  //Exercise 6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (n1, rand1) = ra(rng)
    val (n2, rand2) = rb(rand1)
    (f(n1, n2), rand2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def bothWithMathInTermsOfFlatMap[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2Flatmap(ra, rb)((_, _))
  def doubleWithMapInTermsOfFlatMap: Rand[Double] = mapFlatmap(_.nextInt)(x => (x: Double) / (Int.MaxValue: Double))

  //Exercise 7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def go(rng: RNG, fs: List[Rand[A]]): (List[A], RNG) = fs match {
      case f :: fs => f(rng) match {
        case (a, nextRng) => (a :: go(nextRng, fs)._1, nextRng)
      }
      case _ => (Nil, rng)
    }
    go(rng, fs)
  }

  //Exercise 8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (n, nextRng) = f(rng)
    g(n)(nextRng)
  }
  //Exercise 9
  def mapFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  //Exercise 9
  def map2Flatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (g, newRng) = flatMap(ra)(a => unit(f.curried(a)))(rng)
    flatMap(rb)(b => unit(g(b)))(newRng)
  }

}

//Exercise 10
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (n, nextState) = this.run(s)
    g(n).run(nextState)
  })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (g, nState) = flatMap(a => State.unit(f.curried(a))).run(s)
    rb.flatMap(b => State.unit(g(b))).run(nState)
  })
}

//Exercise 10
object State {
  def unit[A, S](a: A): State[S, A] = State(S => (a, S))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
    def go(s: S, fs: List[State[S, A]]): (List[A], S) = fs match {
      case f :: fs => f.run(s) match {
        case (a, ns) => (a :: go(ns, fs)._1, ns)
      }
      case _ => (Nil, s)
    }
    go(s, fs)
  })

}

//Exercise 11
sealed trait Input
case object Coin extends Input
case object Turn extends Input
//Exercise 11
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  //TODO Use flatmap for turn/insert and chain states? I think I basically cheated by compressing all of the state changes.
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {
    def insertCoin(x: Input, s: ((Int, Int), Machine)): ((Int, Int), Machine) = {
      val ((candies, coins), machine) = s;
      ((candies, coins + 1), Machine(machine.candies <= 0, candies, coins + 1))
    }

    def turnKnob(x: Input, s: ((Int, Int), Machine)): ((Int, Int), Machine) = {
      val ((candies, coins), machine) = s;
      if (candies > 0 && !machine.locked) {
        ((candies - 1, coins), Machine(true, candies - 1, coins))
      } else {
        ((candies, coins), Machine(machine.locked, candies, coins))
      }
    }

    def simulateMachineInternal(inputs: List[Input], s: ((Int, Int), Machine)): ((Int, Int), Machine) = inputs match {
      case i :: is => i match {
        case Coin => simulateMachineInternal(is, insertCoin(i, s))
        case Turn => simulateMachineInternal(is, turnKnob(i, s))
      }
      case _ => s
    }
    simulateMachineInternal(inputs, ((m.candies, m.coins), m));
  })

}

object StateExercises extends App {
  import RNG._

  val exercise1 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    val (n1, rng2) = nonNegativeInt(rng)
    val (n2, _) = nonNegativeInt(rng2)
    println(s"Non negative integer ${n1}")
    println(s"Non negative integer ${n2}")
  }

  val exercise2 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    val (n1, rng2) = doubleNoMap(rng)
    val (n2, _) = doubleNoMap(rng2)
    println(s"Double bt 0 and 1 ${n1}")
    println(s"Double bt 0 and 1 ${n2}")
  }

  val exercise3 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    println(s"Int double ${intDouble(rng)}")
    println(s"Double int ${doubleInt(rng)}")
    println(s"3 Double ${double3(rng)}")
  }

  val exercise4 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    println(s"List of random values ${ints(10)(rng)}")
    println(s"List of random values ${intsBetter(10)(rng)}")
  }

  val exercise5 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    val rngfx: Rand[Int] = _.nextInt
    println(s"Double in terms of map ${double(rng)}")
  }

  val exercise6 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    val rngfx: Rand[Int] = _.nextInt
    println(s"Using both to demonstrate map2 ${both(int, double)(rng)}");
  }

  val exercise7 = {
    val l = List.fill(100)(int)
    val rng = SimpleRNG(System.currentTimeMillis)
    println(s"Sequence ${sequence(l)(rng)}");
  }

  val exercise8 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })
    println(s"Using flatmap... ${nonNegativeLessThan(100)(rng)}");
  }

  val exercise9 = {
    val rng = SimpleRNG(System.currentTimeMillis)
    val rngfx: Rand[Int] = _.nextInt
    println(s"Both first implementation ${both(int, double)(rng)}")
    println(s"Both flatmap implementation ${bothWithMathInTermsOfFlatMap(int, double)(rng)}")
    println(s"Double first implementation ${double(rng)}")
    println(s"Double flatmap implementation ${doubleWithMapInTermsOfFlatMap(rng)}")
  }

  val exercise10 = {
    def int: State[RNG, Int] = State(_.nextInt)
    val rng = SimpleRNG(System.currentTimeMillis)
    println(int.map(x => x + 100).run(rng))
  }

  val exercise11 = {
    val machine = Machine(true, 5, 10)
    println(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(machine))
  }
}



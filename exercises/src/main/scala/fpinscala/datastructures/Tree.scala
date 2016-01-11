package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  object Tree {
  
    //Exercise 25
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 1
      case Branch(x, y) => 1 + size(x) + size(y)
    }

    //Exercise 26
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x)      => x
      case Branch(x, y) => maximum(x) max maximum(y)
    }

    //Exercise 27
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 0
      case Branch(x, y) => (depth(x) max depth(y)) + 1
    }

    //Exercise 28
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x)      => Leaf(f(x))
      case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    }

    //Exercise 29
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(x)      => f(x)
      case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
    }
    
    //Exercise 29
    def sizeFold[A](t: Tree[A]): Int = fold(t)(x => 1)(_ + _ + 1)

    //Exercise 29
    def maximumFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

    //Exercise 29
    def depthFold(t: Tree[Int]): Int = fold(t)(x => 0)((x, y) => (x max y) + 1)

    //Exercise 29
    def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

  }

object TreeExercise extends App {
    val exercise25 = {
    val tree = Branch(Branch(Leaf(13), Leaf(3)), Leaf(45))
    println(s"Number of nodes ${Tree.size(tree)}")

    val tree2 = Branch(Leaf(1), Leaf(45))
    println(s"Number of nodes ${Tree.size(tree2)}")

    val tree3 = Leaf(1)
    println(s"Number of nodes ${Tree.size(tree3)}")
  }

  val exercise26 = {
    val tree = Branch(Branch(Leaf(100), Leaf(3)), Leaf(45))
    println(s"Max of tree is  ${Tree.maximum(tree)}")
  }

  val exercise27 = {
    val tree1 = Branch(Branch(Leaf(100), Branch(Leaf(5), Leaf(3))), Leaf(45))
    println(s"Depth of tree is  ${Tree.depth(tree1)}")

    val tree2 = Branch(Leaf(1), Leaf(2))
    println(s"Depth of tree is  ${Tree.depth(tree2)}")
  }

  val exercise28 = {
    val tree = Branch(Branch(Leaf(100), Branch(Leaf(5), Leaf(3))), Leaf(45))
    println(s"Mapped tree with 10 added is  ${Tree.map(tree)(_ + 10)}")
  }

  val exercise29 = {
    val tree1 = Branch(Branch(Leaf(13), Leaf(3)), Leaf(45))
    println(s"Number of nodes ${Tree.sizeFold(tree1)}")
    val tree2 = Branch(Branch(Leaf(100), Leaf(71000)), Leaf(33))
    println(s"Max of tree is  ${Tree.maximumFold(tree2)}")
    val tree3 = Branch(Branch(Leaf(100), Branch(Branch(Leaf(5), Leaf(3)), Leaf(3))), Leaf(45))
    println(s"Depth of tree is  ${Tree.depthFold(tree3)}")
    val tree4 = Branch(Branch(Leaf(100), Branch(Leaf(5), Leaf(3))), Leaf(45))
    println(s"Mapped tree with 10 multiplied is  ${Tree.mapFold(tree4)(_ * 10)}")
  }
}
package ch3.first

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximium(tree: Tree[Int]): Int = {
    tree match {
      case Branch(l, r) => maximium(l) max maximium(r)
      case Leaf(v) => v
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((l, r) => 1 + l + r)
  }

  def maxViaFold(tree: Tree[Int]): Int = {
    fold(tree)(v => v)((v1, v2) => v1 max v2)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 0)((l, r) => 1 + (l max r))
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)):Tree[B])((l,r) => Branch(l,r))
  }

  def main(args: Array[String]): Unit = {
    val tree1 = Branch(Branch(Branch(Leaf(11), Leaf(1)), Leaf(1)), Leaf(2))
    println(depthViaFold(tree1))
  }

}
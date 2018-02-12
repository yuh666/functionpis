package ch3.second

import com.sun.xml.internal.ws.model.AbstractWrapperBeanGenerator.BeanMemberFactory

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
      case Leaf(v) => v
      case Branch(l, r) => maximium(l) max maximium(r)
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

  def sizeViaFold[A](tree: Tree[A]) = {
    fold(tree)(_ => 1)(1 + _ + _)
  }

  def depthViaFold[A](tree: Tree[A]) = {
    fold(tree)(_ => 0)((a, b) => 1 + (a max b))
  }

  def maximiumViaFold(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }


  /**
    * curry类型推断取决于第一个参数
    * @param tree
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
  }


  def main(args: Array[String]): Unit = {
    val tree1 = Branch(Branch(Branch(Leaf(11), Leaf(1)), Leaf(1)), Leaf(2))
    println(maximiumViaFold(tree1))
  }

}
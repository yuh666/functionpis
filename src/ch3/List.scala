package ch3

import scala.util.control.TailCalls.TailRec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case Cons(s, xs) => s + sum(xs)
    }
  }

  def product(list: List[Double]): Double = {
    list match {
      case Nil => 1.0
      case Cons(0.0, tail) => 0.0
      case Cons(s, xs) => s * product(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val ext1 = List(1, 2, 3)
    val ext2 = Cons(1, Cons(2, Nil))
    val ext3 = Cons(1, Nil)
    val ext4 = Nil
    println(product(List(1,2,3,4,5)))
    println(sum(List(1,2,3,4,5)))
  }

}
package ch3

import scala.collection.immutable.Stream.Empty
import scala.collection.mutable.ListBuffer
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


  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](list: List[A], newHead: A): List[A] = {
    list match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, tail) => Cons(newHead, tail)
    }
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list else {
      list match {
        case Nil => sys.error("tail of empty list")
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }
  }

  /**
    * List的移位递归处理 处理某个不确定的元素 case后面可以可以继续组装
    *
    * 使用柯理化以助于类型的推导
    *
    * @param list
    * @param f
    * @tparam A
    * @return
    */
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = {
    list match {
      case Nil => list
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case Cons(head, tail) => Cons(head, dropWhile(tail)(f))
    }
  }

  def append[A](a: List[A], b: List[A]): List[A] = {
    a match {
      case Nil => b
      case Cons(h, t) => Cons(h, append(t, b))
    }
  }


  def init[A](list: List[A]): List[A] = {
    list match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def init2[A](list: List[A]): List[A] = {
    import scala.collection.mutable.ListBuffer
    val buffer = new ListBuffer[A]()

    def go(list: List[A]): List[A] = {
      list match {
        case Nil => sys.error("empty list")
        case Cons(_, Nil) => List(buffer.toList: _*)
        case Cons(h, t) => buffer += h; go(t)
      }
    }

    go(list)
  }


  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight1(t, z)(f))
    }
  }

  def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft1(t, f(z, h))(f)
    }
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /**
    * 尾递归版本左折叠 谁直接传入函数就先计算谁
    *
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def product3(as: List[Double]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }

  def sum2(as: List[Int]): Int = {
    foldRight(as, 0)(_ + _)
  }

  def product2(as: List[Double]): Double = {
    foldRight(as, 1.0)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, j) => j + 1)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((i, _) => i + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((i, j) => Cons(j, i))
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_, _))
  }

  def concat[A](list: List[List[A]]): List[A] = {
    foldLeft(list, Nil: List[A])(append)
  }

  def increment(list: List[Int]): List[Int] = {
    foldRight(list, List[Int]())((i, j) => Cons(i + 1, j))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((i, j) => Cons(f(i), j))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A]) { (i, j) =>
      if (f(i)) Cons(i, j) else j
    }
  }

  def filter_1[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = new ListBuffer[A]()

    def go(as: List[A]): List[A] = {
      as match {
        case Nil => as
        case Cons(h, t) if (f(h)) => buf += h; go(t)
        case Cons(_, t) => go(t)
      }
    }

    go(as)
    List(buf.toList: _*)
  }


  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def plusList(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h, t), Cons(h1, t1)) => Cons(h1 + h, plusList(t, t1))
    }
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h, t), Cons(h1, t1)) => Cons(f(h, h1), zipWith(t, t1)(f))
    }
  }


  def filter_flatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(i => if (f(i)) List(i) else Nil)
  }


  /**
    * Right -> 从右一压栈到最后倒着执行运算
    * Left -> 从左一运算
    * 然后压栈
    *
    * @param l
    * @param r
    * @tparam A
    * @return
    */
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = {
    foldLeft(l, r)((i, j) => Cons(j, i))
  }

  def dts(list: List[Double]): List[String] = {
    foldRight(list, List[String]())((i, j) => (Cons(i.toString, j)))
  }

  @annotation.tailrec
  def startsWith[A](sup: List[A], pre: List[A]): Boolean = {
    (sup, pre) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
      case _ => false
    }
  }

  @annotation.tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if (startsWith(sup, sub)) => true
      case Cons(h, t) => hasSubSequence(t, sub)
    }
  }


  @annotation.tailrec
  def startsWith1[A](sup: List[A], sub: List[A]): Boolean = {
    (sub, sup) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith1(t1, t2)
      case _ => false
    }
  }

  @annotation.tailrec
  def hasSubsequence1[A](sup:List[A],sub:List[A]):Boolean={
    sup match {
      case Nil => sub == Nil
      case _ if(startsWith1(sub,sub)) => true
      case Cons(h,t) => hasSubsequence1(t,sub)
    }
  }


  def main(args: Array[String]): Unit = {
    println(filter_1(List(1, 2, 3))(_ == 1))
    //println(dts(List(1, 2, 3, 4)))
    //println(increment(List(1, 2, 3, 4)))
    //println(length2(List(1, 2, 3, 4)))
    //val unit = foldRight(List(1,2,3,4),Nil:List[Int])(Cons(_,_))
    //println(unit)
    //val ext1 = List(1, 2, 3)
    //val ext2 = Cons(1, Cons(2, Nil))
    //val ext3 = Cons(1, Nil)
    //val ext4 = Nil
    //println(product3(List(1, 2, 3, 4, 5)))
    //println(sum3(List(1, 2, 3, 4, 5)))
    //val a = ext1 match {
    //  case Nil => 1
    //  case _ => 2
    //}
    //val x = List(1, 2, 3, 4, 5) match {
    //  case Cons(x, Cons(2, Cons(4, _))) => x
    //  case Nil => 42
    //  //case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //case Cons(h, t) => h + sum(t)
    //case _ =>
    //  101
    // }
    //println(dropWhile(ext1)(i => i == 1 || i == 3))
    //println(append(ext1, ext2))

  }

}
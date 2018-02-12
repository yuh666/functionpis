package ch3.second

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => sys.error("empty list")
      case Cons(h, t) => t
    }
  }

  def setHead[A](as: List[A], nh: A): List[A] = {
    as match {
      case Nil => sys.error("empty")
      case Cons(h, t) => Cons(nh, t)
    }
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    as match {
      case Nil => sys.error("empty list")
      case Cons(h, t) if (n > 0) => drop(t, n - 1)
      case _ => as
    }
  }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => sys.error("empty list")
      case Cons(h, t) if (f(h)) => dropWhile(t)(f)
      case _ => as
    }
  }

  def init[A](as: List[A]): List[A] = {
    as match {
      case Nil => sys.error("empty")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def sum(as: List[Int]): Int = {
    foldRight(as, 0)(_ + _)
  }

  def product(as: List[Int]): Int = {
    foldRight(as, 1)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, z) => z + 1)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sum1(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def product1(as: List[Int]): Int = {
    foldLeft(as, 1)(_ * _)
  }

  def append[A](as: List[A], as2: List[A]): List[A] = {
    as match {
      case Nil => as2
      case _ => foldLeft(as, as2)((h, l) => Cons(l, h))
    }
  }

  def append2[A](as: List[A], as2: List[A]): List[A] = {
    as match {
      case Nil => as2
      case _ => foldRight(as, as2)((h, l) => Cons(h, l))
    }
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])(append2(_, _))
  }

  def increment(as: List[Int]): List[Int] = {
    foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  def toString(as: List[Int]): List[String] = {
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString(), b))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def plus(as: List[Int], as2: List[Int]): List[Int] = {
    (as, as2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, plus(t1, t2))
    }
  }

  def zipWith[A, B, C](as: List[A], as2: List[B])(f: (A, B) => C): List[C] = {
    (as, as2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }


  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
      case _ => false
    }
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t1) => hasSubSequence(t1, sub)
    }
  }


  def main(args: Array[String]): Unit = {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(1, 2, 3, 4, 6)
    val lst3 = List(lst1, lst2)
    println(filter1(concat(lst3))(_ == 1))

  }


}
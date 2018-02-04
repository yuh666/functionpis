package ch2

object TailRecDemo extends App {

  /**
    * 阶乘
    *
    * @param n
    * @return
    */
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, acc * n)

    go(n, 1)
  }

  /**
    * 斐波那契数列
    *
    * @param n
    * @return
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      n match {
        case 1 => b
        case _ => go(b, a + b, n - 1)
      }
    }

    go(0, 1, n)
  }

  /**
    * 非优化斐波那契数列
    *
    * @param n
    * @return
    */
  def fib1(n: Int): Int = if (n <= 2) 1 else fib1(n - 1) + fib1(n - 2)

  /**
    * HOF范例
    *
    * @param name
    * @param n
    * @param f
    * @return
    */
  def msg(name: String, n: Int, f: Int => Int): String = {
    val msg = s"the $name of $n is %d"
    msg.format(f(n))
  }

  /**
    * 顺序查找字符串出现的位置
    *
    * @param arr
    * @param p
    * @tparam A
    * @return
    */
  def findFirstIndex[A](arr: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= arr.length) -1
      else if (p(arr(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * 检查是否是有序的
    * @param arr
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    var res = true
    for (i <- 0 until arr.length if ((i + 1) < arr.length && ordered(arr(i),arr(i+1)))) res = false
    res
  }


  /**
    * 检查是否是有序的 递归就是在此步返回或者在以后的某一步返回
    * @param arr
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted1[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n:Int):Boolean={
      n match {
        case n if n > arr.length-1 => true
        case n if(!ordered(arr(n),arr(n+1))) => false
        case _ => go(n+1)
      }
    }
    go(0)
  }


  //println(msg("fib",10,fib))
  //println(findFirstIndex(Array("a", "b"), (i: String) => i == "c"))
  //println(isSorted[String](Array("a", "b", "c", "d","A"), (a, b) => a < b))
  println(isSorted[String](Array("a", "b", "c", "d","A"), (a, b) => a < b))

}

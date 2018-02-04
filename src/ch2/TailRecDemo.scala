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
    * @param n
    * @return
    */
  def fib1(n: Int): Int = if (n <= 2) 1 else fib1(n - 1) + fib1(n - 2)

  /**
    * HOF范例
    * @param name
    * @param n
    * @param f
    * @return
    */
  def msg(name:String,n:Int,f:Int=>Int): String ={
    val msg = s"the $name of $n is %d"
    msg.format(f(n))
  }

  println(msg("fib",10,fib))

}

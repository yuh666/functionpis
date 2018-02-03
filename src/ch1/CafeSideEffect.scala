package ch1

/**
  * @Author yuh 
  * @Date Created in 上午11:25 2018/2/2 
  * @Description cafe转移副作用的例子
  */
object CafeSideEffect extends App {

  /**
    * 信用卡💳
    */
  class CreditCard()

  /**
    * 咖啡实例
    *
    * @param price
    */
  case class Coffee(price: Double = 30)

  /**
    * 包装消费实体
    *
    * @param creditCard
    * @param amount
    */
  case class Charge(creditCard: CreditCard, amount: Double) {
    def combine(other: Charge) = Charge(creditCard, this.amount + other.amount)
  }

  /**
    * 咖啡店
    */
  class Cafe {
    /**
      * 买一杯咖啡
      *
      * @param creditCard
      * @return
      */
    def buyCoffee(creditCard: CreditCard): (Coffee, Charge) = {
      val cup = new Coffee()
      (cup, Charge(creditCard, cup.price))
    }

    /**
      * 买n杯咖啡
      *
      * @param creditCard
      * @param n
      * @return
      */
    def bufCoffees(creditCard: CreditCard, n: Int): (List[Coffee], Charge) = {
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(creditCard))
      val (coffees, charges) = purchases.unzip
      (coffees, charges.reduce(_ combine _))
    }

  }

  /**
    * 算总账
    *
    * @param charges
    * @return
    */
  def coalesce(charges: List[Charge]): List[Charge] = {
    charges.groupBy(_.creditCard).values.map(_.reduce(_ combine _)).toList
  }

  val card = new CreditCard()
  val cafe = new Cafe()
  val charges = List(cafe.bufCoffees(card, 10)._2, cafe.bufCoffees(card, 10)._2)
  println(coalesce(charges))
}

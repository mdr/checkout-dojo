package dojo

object Data {

  val Apple = Item(30)
  val Beans = Item(50)
  val Coke = Item(180)
  val Deodorant = Item(250)
  val Egg = Item(120)

  val AllDiscounts = {
    val appleDiscount = DiscountGroup(Map(Apple -> 4), 20)
    val deodorantDiscount = DiscountGroup(Map(Deodorant -> 2), 50)
    val eggDiscount = DiscountGroup(Map(Egg -> 3), 60)
    val cokeBeanDiscount = DiscountGroup(Map(Coke -> 1, Beans -> 1), 30)
    Seq(appleDiscount, deodorantDiscount, cokeBeanDiscount, eggDiscount)
  }

}

case class Item(cost: Int)

case class Basket(items: Map[Item, Int])

case class DiscountGroup(items: Map[Item, Int], discountAmount: Int) {

  /**
   * @return true iff this discount is applicable to the given basket
   */
  def applies(basket: Basket): Boolean =
    items.forall { case (item, count) ⇒ basket.items.getOrElse(item, 0) >= count }

  /**
   * @return a new basket with all the discounted items removed
   */
  def apply(basket: Basket): Basket =
    Basket(basket.items.map { case (item, count) ⇒ item -> (count - items.getOrElse(item, 0)) })

}

object CheckoutCalculator {
  
  /**
   * Calculate total checkout price for a collection of items
   */
  def total(items: Item*): Int = items.map(_.cost).sum - discount(items)

  private def getDiscounts(basket: Basket): Seq[DiscountGroup] = {
    for (discount ← Data.AllDiscounts if discount.applies(basket))
      return discount +: getDiscounts(discount.apply(basket))
    Seq() // No discounts
  }

  private def discount(items: Seq[Item]): Int = {
    val basket = Basket(items.groupBy(identity).mapValues(_.length))
    getDiscounts(basket).map(_.discountAmount).sum
  }

}
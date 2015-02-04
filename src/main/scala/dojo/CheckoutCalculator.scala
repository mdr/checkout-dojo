package dojo

sealed trait Item { val cost: Int }
case object Apple extends Item { val cost: Int = 30 }
case object Beans extends Item { val cost: Int = 50 }
case object Coke extends Item { val cost: Int = 180 }
case object Deodorant extends Item { val cost: Int = 250 }
case object Egg extends Item { val cost: Int = 120 }

case class DiscountGroup(items: Map[Item, Int], discountAmount: Int) {

  def applies(itemCounts: Map[Item, Int]): Boolean =
    items.forall { case (item, count) ⇒ itemCounts.getOrElse(item, 0) >= count }

  def apply(itemCounts: Map[Item, Int]): Map[Item, Int] =
    itemCounts.map { case (item, count) ⇒ item -> (count - items.getOrElse(item, 0)) }

}

object CheckoutCalculator {

  /**
   * Calculate total checkout price for a collection of items
   */
  def total(items: Item*): Int = items.map(_.cost).sum - discount(items)

  private val AllDiscounts = {
    val appleDiscount = DiscountGroup(Map(Apple -> 4), 20)
    val deodorantDiscount = DiscountGroup(Map(Deodorant -> 2), 50)
    val eggDiscount = DiscountGroup(Map(Egg -> 3), 60)
    val cokeBeanDiscount = DiscountGroup(Map(Coke -> 1, Beans -> 1), 30)
    Seq(appleDiscount, deodorantDiscount, cokeBeanDiscount, eggDiscount)
  }

  private def getDiscounts(itemCounts: Map[Item, Int]): Seq[DiscountGroup] = {
    for (discount ← AllDiscounts if discount.applies(itemCounts))
      return discount +: getDiscounts(discount.apply(itemCounts))
    Seq() // No discounts
  }

  private def discount(items: Seq[Item]): Int = {
    val itemCounts: Map[Item, Int] = items.groupBy(identity).mapValues(_.length)
    getDiscounts(itemCounts).map(_.discountAmount).sum
  }

}
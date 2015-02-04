package dojo

sealed trait Item
case object Apple extends Item
case object Beans extends Item
case object Coke extends Item
case object Deodorant extends Item
case object Egg extends Item

case class DiscountGroup(items: Map[Item, Int], discountAmount: Int) {

  def applies(itemCounts: Map[Item, Int]): Boolean =
    items.forall { case (item, count) ⇒ itemCounts.getOrElse(item, 0) >= count }

  def apply(itemCounts: Map[Item, Int]): Map[Item, Int] =
    itemCounts.map { case (item, count) ⇒ item -> (count - items.getOrElse(item, 0)) }

}

object CheckoutCalculator {

  val AllDiscounts = {
    val appleDiscount = DiscountGroup(Map(Apple -> 4), 20)
    val deodorantDiscount = DiscountGroup(Map(Deodorant -> 2), 50)
    val eggDiscount = DiscountGroup(Map(Egg -> 3), 60)
    val cokeBeanDiscount = DiscountGroup(Map(Coke -> 1, Beans -> 1), 30)
    Seq(appleDiscount, deodorantDiscount, cokeBeanDiscount, eggDiscount)
  }

  def total(items: Item*): Int = items.map(basePrice).sum - discount(items)

  def getDiscounts(itemCounts: Map[Item, Int]): Seq[DiscountGroup] = {
    for (discount ← AllDiscounts if discount.applies(itemCounts))
      return discount +: getDiscounts(discount.apply(itemCounts))
    Seq() // No discounts
  }
  def discount(items: Seq[Item]): Int = {
    val itemCounts: Map[Item, Int] = items.groupBy(identity).mapValues(_.length)
    getDiscounts(itemCounts).map(_.discountAmount).sum
  }

  def discountQuantity(item: Item): Int = item match {
    case Apple     ⇒ 4
    case Beans     ⇒ 1
    case Coke      ⇒ 1
    case Deodorant ⇒ 2
    case Egg       ⇒ 3
  }

  def discountAmount(item: Item): Int = item match {
    case Apple     ⇒ 20
    case Beans     ⇒ 0
    case Coke      ⇒ 0
    case Deodorant ⇒ 50
    case Egg       ⇒ 60
  }

  def basePrice(item: Item): Int = item match {
    case Apple     ⇒ 30
    case Beans     ⇒ 50
    case Coke      ⇒ 180
    case Deodorant ⇒ 250
    case Egg       ⇒ 120
  }

}
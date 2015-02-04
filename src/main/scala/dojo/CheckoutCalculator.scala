package dojo

sealed trait Item
case object Apple extends Item
case object Beans extends Item
case object Coke extends Item
case object Deodorant extends Item
case object Egg extends Item

object CheckoutCalculator {

  def total(items: Item*): Int = items.map(rawPrice).sum - discount(items)

  def discount(items: Seq[Item]): Int = {
    val itemCounts = items.groupBy(identity).mapValues(_.length)
    def calculateDiscount(item: Item, count: Int) = discountAmount(item) * (count / discountQuantity(item))
    itemCounts.map { case (item, count) ⇒ calculateDiscount(item, count) }.sum
  }

  def discountBAD(items: Seq[Item]): Int =
    items.groupBy(identity).mapValues(_.length).map { case (item, count) ⇒ discountAmount(item) * (count / discountQuantity(item)) }.sum

  def discountQuantity(item: Item): Int = item match {
    case Apple     ⇒ 4
    case Beans     ⇒ 1
    case Coke      ⇒ 1
    case Deodorant ⇒ 2
  }

  def discountAmount(item: Item): Int = item match {
    case Apple     ⇒ 20
    case Beans     ⇒ 0
    case Coke      ⇒ 0
    case Deodorant ⇒ 50
  }

  def rawPrice(item: Item): Int = item match {
    case Apple     ⇒ 30
    case Beans     ⇒ 50
    case Coke      ⇒ 180
    case Deodorant ⇒ 250
  }

}
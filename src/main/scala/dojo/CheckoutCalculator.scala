package dojo

sealed trait Item
case object Apple extends Item
case object Beans extends Item
case object Coke extends Item
case object Deodorant extends Item

object CheckoutCalculator {

  def total(items: Item*): Int = items.map(rawPrice).sum - discount(items)

  def discount(items: Seq[Item]): Int = {
    val itemCounts = items.groupBy { identity }.mapValues { x ⇒ x.length }
    val appleCounts = itemCounts.getOrElse(Apple, 0)
    val appleDiscount = 20 * (appleCounts / 4)
    appleDiscount
  }

  def rawPrice(item: Item): Int = item match {
    case Apple     ⇒ 30
    case Beans     ⇒ 50
    case Coke      ⇒ 180
    case Deodorant ⇒ 250
  }

}
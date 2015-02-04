package dojo

sealed trait Item
case object Apple extends Item
case object Beans extends Item
case object Coke extends Item
case object Deodorant extends Item

object CheckoutCalculator {

  def total(items: Item*): Int = items.map(rawPrice).sum - discount(items)
  
  def discount(items: Seq[Item]): Int = {
    val itemCounts = items.groupBy {identity}.mapValues {x => x.length}
    if (itemCounts.getOrElse(Apple, 0) >= 4) {
      return 20
    } else {
      return 0
    }
  }

  def rawPrice(item: Item): Int = item match {
    case Apple     ⇒ 30
    case Beans     ⇒ 50
    case Coke      ⇒ 180
    case Deodorant ⇒ 250
  }

}
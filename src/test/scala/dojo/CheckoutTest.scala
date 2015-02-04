package dojo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import dojo.CheckoutCalculator._

// Apple £0.30   4 for £1.00
// Beans £0.50   
// Coke  £1.80
// Deo   £2.50   2 for £4.50

class CheckoutTest extends FlatSpec with ShouldMatchers {

  "Checkout" should "work" in {

    total(Apple) should equal(30)

  }

}
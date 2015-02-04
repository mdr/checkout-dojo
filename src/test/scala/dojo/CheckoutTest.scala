package dojo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import dojo.CheckoutCalculator._

// Apple £0.30   4 for £1.00
// Beans £0.50   
// Coke  £1.80
// Deo   £2.50   2 for £4.50

class CheckoutTest extends FlatSpec with ShouldMatchers {

  "Checkout a single item" should "give the correct total for just that item" in {

    total(Apple) should equal(30)

  }

  "Checkout two items" should "cost the correct amount" in {

    total(Apple, Beans) should equal(80)

  }

  "Checking out four apples" should "apply a discount" in {

    total(Apple, Apple, Apple, Apple) should equal(100)

  }

  "Checking out 13 apples" should "apply three discounts" in {

    total(Apple, Apple, Apple, Apple, Apple, Apple, Apple, Apple, Apple, Apple, Apple, Apple, Apple) should equal(330)

  }

  "Checking out 5 deodorants apples" should "apply two discounts" in {

    total(Deodorant, Deodorant, Deodorant, Deodorant, Deodorant) should equal(1150)

  }

}
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import us.gnos.enstrophy._

class SortTest extends FunSpec with ShouldMatchers {

  describe("ExchangeSortFunctional") {

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values") {
        val intArray = Array(7,99,1,8,10,3)
        val result = ExchangeSortFunctional.sort(intArray)
        result should equal (Array(1,3,7,8,10,99))
      }
    }
  }

  describe("ExchangeSort") {

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values") {
        var intArray = Array(7,99,1,8,10,3)
        val result = ExchangeSortFunctional.sort(intArray)
        result should equal (Array(1,3,7,8,10,99))
      }
    }
  }

}


package us.gnos.enstrophy

import org.scalatest._

import us.gnos.enstrophy.sort._

class SortTest extends FunSpec with ShouldMatchers with BeforeAndAfter {

  val intArray = Array(7,99,1,8,10,3)
  val sortedIntArray = Array(1,3,7,8,10,99)
  val hintArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
  val h3sortedIntArray = Array(7,10,1,8,32,3,20,99,21,55,777,89)
  val hsortedIntArray = Array(1,3,7,8,10,20,21,32,55,89,99,777)

  describe("ExchangeSort") {

    object ExchangeSortTest extends Tag("us.gnos.estrophy.SortTest.ExchangeSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", ExchangeSortTest) {
        ExchangeSortFunctional.sort(intArray) should equal (sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", ExchangeSortTest) {
          ExchangeSortFunctional.sort(intArray) should equal (sortedIntArray)
        }
      }
    }
  }

  describe("InsertionSort") {

    object InsertionSortTest extends Tag("us.gnos.estrophy.SortTest.InsertionSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", InsertionSortTest) {
        InsertionSort.sort(intArray) should equal (sortedIntArray)
      }
    }
    describe("hsort") {
      it("returns an h sorted Array[Int] when given an Array[Int] with ramdom values and an h value", InsertionSortTest) {
        InsertionSort.hsort(hintArray, 3) should equal (h3sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", InsertionSortTest) {
          InsertionSortFunctional.sort(intArray) should equal (sortedIntArray)
        }
      }
      describe("hsort") {
        it("returns an h sorted Array[Int] when given an Array[Int] with ramdom values and an h value", InsertionSortTest) {
          InsertionSortFunctional.hsort(hintArray, 3) should equal (h3sortedIntArray)
        }
      }
    }
  }

  describe("InsertionSortWithoutExchange") {

    object InsertionSortWithoutExchagesTest extends Tag("us.gnos.estrophy.SortTest.InsertionSortWithoutExchagesTests")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", InsertionSortWithoutExchagesTest) (pending)
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", InsertionSortWithoutExchagesTest) (pending)
      }
    }

  }

  describe("ShellSort") {

    object ShellSortTest extends Tag("us.gnos.estrophy.SortTest.ShellSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", ShellSortTest) {
        ShellSort.sort(hintArray) should equal (hsortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with ramdom values", ShellSortTest) {
          ShellSortFunctional.sort(hintArray) should equal (hsortedIntArray)
        }
      }
    }
  }

}


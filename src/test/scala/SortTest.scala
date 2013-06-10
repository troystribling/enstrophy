package us.gnos.enstrophy

import org.scalatest._

import us.gnos.enstrophy.sort._

class SortTest extends FunSpec with ShouldMatchers with BeforeAndAfter {

  var intArray : Array[Int] = _
  var sortedIntArray = Array(1,3,7,8,10,99)

  var hintArray : Array[Int] = _
  var h3sortedIntArray = Array(7,10,1,8,32,3,20,99,21,55,777,89)

  var shellIntArray : Array[Int] = _
  var shellSortedIntArray = Array(1,3,7,8,10,20,21,32,55,89,99,777)

  var mergeIntArray : Array[Int] = _
  var mergedIntArray = Array(6,5,2,3,5,6,7,7,9,10,1,4,7)
  var mergeSortedIntArray = Array(1,2,3,4,5,5,6,6,7,7,7,9,10)

  before {
    intArray = Array(7,99,1,8,10,3)
    hintArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
    shellIntArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
    mergeIntArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
  }

  describe("ExchangeSort") {

    object ExchangeSortTest extends Tag("us.gnos.estrophy.SortTest.ExchangeSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", ExchangeSortTest) {
        ExchangeSortFunctional.sort(intArray) should equal (sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with random values", ExchangeSortTest) {
          ExchangeSortFunctional.sort(intArray) should equal (sortedIntArray)
        }
      }
    }
  }

  describe("InsertionSort") {

    object InsertionSortTest extends Tag("us.gnos.estrophy.SortTest.InsertionSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", InsertionSortTest) {
        InsertionSort.sort(intArray) should equal (sortedIntArray)
      }
    }
    describe("hsort") {
      it("returns an h sorted Array[Int] when given an Array[Int] with random values and an h value", InsertionSortTest) {
        InsertionSort.hsort(hintArray, 3) should equal (h3sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with random values", InsertionSortTest) {
          InsertionSortFunctional.sort(intArray) should equal (sortedIntArray)
        }
      }
      describe("hsort") {
        it("returns an h sorted Array[Int] when given an Array[Int] with random values and an h value", InsertionSortTest) {
          InsertionSortFunctional.hsort(hintArray, 3) should equal (h3sortedIntArray)
        }
      }
    }
  }

  describe("InsertionSortWithoutExchange") {

    object InsertionSortWithoutExchagesTest extends Tag("us.gnos.estrophy.SortTest.InsertionSortWithoutExchagesTests")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", InsertionSortWithoutExchagesTest) (pending)
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with random values", InsertionSortWithoutExchagesTest) (pending)
      }
    }

  }

  describe("ShellSort") {

    object ShellSortTest extends Tag("us.gnos.estrophy.SortTest.ShellSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", ShellSortTest) {
        ShellSort.sort(intArray) should equal (sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with random values", ShellSortTest) {
          ShellSortFunctional.sort(intArray) should equal (sortedIntArray)
        }
      }
    }
  }

  describe("SortUtils") {

    object SortUtilsTest extends Tag("us.gnos.enstropy.SortTest.SortUtils")

    describe("hmax") {
      it ("returns the maximum h value used by shell sort when given an array size", SortUtilsTest) {
        SortUtils.hmax(1) should equal (1)
        SortUtils.hmax(10) should equal (4)
        SortUtils.hmax(100) should equal (40)
        SortUtils.hmax(1000) should equal (364)
      }
    }

    describe("merge") {
      it("returns a single sorted Array[Int] when given two serted Array[Int]s", SortUtilsTest) {
        SortUtils.merge(input = mergeIntArray,
                        tmp = new Array[Int](mergeIntArray.length),
                        lo = 2, mid = 5, hi = 9) should equal(mergedIntArray)
      }
    }

  }

  describe("MergeSort") {

    object MergeSortTest extends Tag("us.gnos.enstrophy.SortTest.MergeSort")

    describe("topDownSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values", MergeSortTest) {
        MergeSort.topDownSort(mergeIntArray) should equal(mergeSortedIntArray)
      }
    }

    describe("bottomUpSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values", MergeSortTest) {
      }
    }

  }
}


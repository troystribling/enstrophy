package us.gnos.enstrophy

import org.scalatest._

import us.gnos.enstrophy.sort._

class SortTest extends FunSpec with ShouldMatchers with BeforeAndAfter {

  describe("ExchangeSort") {

    var sortedIntArray = Array(1,3,7,8,10,99)

    object ExchangeSortTest extends Tag("us.gnos.estrophy.SortTest.ExchangeSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", ExchangeSortTest) {
        val intArray = Array(7,99,1,8,10,3)
        ExchangeSortFunctional.sort(intArray) should equal (sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with random values", ExchangeSortTest) {
          val intArray = Array(7,99,1,8,10,3)
          ExchangeSortFunctional.sort(intArray) should equal (sortedIntArray)
        }
      }
    }
  }

  describe("InsertionSort") {

    object InsertionSortTest extends Tag("us.gnos.estrophy.SortTest.InsertionSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", InsertionSortTest) {
        var sortedIntArray = Array(1,3,7,8,10,99)
        var intArray = Array(7,99,1,8,10,3)
        InsertionSort.sort(intArray) should equal (sortedIntArray)
      }
    }
    describe("hsort") {
      it("returns an h sorted Array[Int] when given an Array[Int] with random values and an h value", InsertionSortTest) {
        var h3sortedIntArray = Array(7,10,1,8,32,3,20,99,21,55,777,89)
        var hintArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
        InsertionSort.hsort(hintArray, 3) should equal (h3sortedIntArray)
      }
    }

    describe("Functional implimentation") {

      describe("sort") {
        it ("returns a sorted List[Int] when given a List[Int] with random values", InsertionSortTest) {
          var sortedIntList = List(1,3,7,8,10,99)
          var intList = List(7,99,1,8,10,3)
          InsertionSortFunctional.sort(intList) should equal (sortedIntList)
        }
      }
      describe("hsort") {
        it("returns an h sorted Array[Int] when given an Array[Int] with random values and an h value", InsertionSortTest) {
          val hintList = List(8,99,1,7,10,89,20,777,21,55,32,3)
          var h3sortedIntList = Array(7,10,1,8,32,3,20,99,21,55,777,89)
          InsertionSortFunctional.hsort(hintList, 3) should equal (h3sortedIntList)
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

    val shellSortedIntArray = Array(1,3,7,8,10,20,21,32,55,89,99,777)

    object ShellSortTest extends Tag("us.gnos.estrophy.SortTest.ShellSortTest")

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values", ShellSortTest) {
        val shellIntArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
        ShellSort.sort(shellIntArray) should equal (shellSortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted Array[Int] when given an Array[Int] with random values", ShellSortTest) {
          val shellIntArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
          ShellSortFunctional.sort(shellIntArray) should equal (shellSortedIntArray)
        }
      }
    }
  }

  describe("SortUtils") {

    object SortUtilsTest extends Tag("us.gnos.enstropy.SortTest.SortUtilsTest")

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
        var mergedIntArray = Array(6,5,2,3,5,6,7,7,9,10,1,4,7)
        var mergeIntArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        SortUtils.merge(input = mergeIntArray,
                        tmp = new Array[Int](mergeIntArray.length),
                        lo = 2, mid = 5, hi = 9) should equal(mergedIntArray)
      }
    }

    describe("mergeFunctional") {
      it("returns a single sorted List[Int] when given two serted List[Int]s", SortUtilsTest) {
        val mergeIntListLeft = List(3,5,6,7,9,10)
        val mergeIntListRight = List(1,2,4,5,6,7,7)
        val mergedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)
        SortUtils.mergeFunctional(mergeIntListLeft, mergeIntListRight) should equal(mergedIntList)
      }
    }
  }

  describe("MergeSort") {

    var mergeSortedIntArray = Array(1,2,3,4,5,5,6,6,7,7,7,9,10)

    object MergeSortTest extends Tag("us.gnos.enstrophy.SortTest.MergeSortTest")

    describe("topDownSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values", MergeSortTest) {
        var mergeIntArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        MergeSort.topDownSort(mergeIntArray) should equal(mergeSortedIntArray)
      }
    }

    describe("bottomUpSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values", MergeSortTest)  (pending)
    }

    describe("Functional Implementation") {

      val mergeSortedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)

      describe("topDownSort") {
        it ("returns a sorted List[Int] when given a List[Int] with random values", MergeSortTest) {
          val mergeIntList = List(6,5,3,7,9,10,2,5,6,7,1,4,7)
          MergeSortFunctional.topDownSort(mergeIntList) should equal(mergeSortedIntList)
        }
      }
    }
  }
}


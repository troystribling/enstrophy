package us.gnos.enstrophy

import org.scalatest._

import us.gnos.enstrophy.sort._

trait SortTest extends FunSpec with ShouldMatchers with BeforeAndAfter

class ExchangeSortTest extends SortTest {

  describe("ExchangeSort") {

    var sortedIntArray = Array(1,3,7,8,10,99)

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values") {
        val intArray = Array(7,99,1,8,10,3)
        ExchangeSort.sort(intArray) should equal (sortedIntArray)
      }
    }
  }
}

class InsertionSortTest extends SortTest {

  var sortedIntArray = Array(1,3,7,8,10,99)
  val sortedIntList = List(1,3,7,8,10,99)

  describe("InsertionSort") {

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values") {
        var intArray = Array(7,99,1,8,10,3)
        InsertionSort.sort(intArray) should equal (sortedIntArray)
      }
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted List[Int] when given a List[Int] with random values") {
          val intList = List(7,99,1,8,10,3)
          InsertionSortFunctional.sort(intList) should equal (sortedIntList)
        }
      }
    }
  }
}

class InsertionSortWithoutExchagesTest extends SortTest {

  describe("InsertionSortWithoutExchange") {

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values") (pending)
    }

    describe("Functional implimentation") {
      describe("sort") {
        it ("returns a sorted List[Int] when given an List[Int] with random values") (pending)
      }
    }

  }
}

class ShellSortTest extends SortTest {

  var sortedIntArray = Array(1,3,7,8,10,20,21,32,55,89,99,777)

  describe("ShellSort") {

    describe("sort") {
      it ("returns a sorted Array[Int] when given an Array[Int] with random values") {
        var intArray = Array(8,99,1,7,10,89,20,777,21,55,32,3)
        ShellSort.sort(intArray) should equal (sortedIntArray)
      }
    }
  }

}

class SortUtilsTest extends SortTest {

  describe("SortUtils") {

    describe("hmax") {
      it ("returns the maximum h value used by shell sort when given an array size") {
        SortUtils.hmax(1) should equal (1)
        SortUtils.hmax(10) should equal (4)
        SortUtils.hmax(100) should equal (40)
        SortUtils.hmax(1000) should equal (364)
      }
    }

    describe("merge") {
      it("returns a single sorted Array[Int] when given two serted Array[Int]s") {
        var mergedIntArray = Array(6,5,2,3,5,6,7,7,9,10,1,4,7)
        var intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        SortUtils.merge(input = intArray,
                        tmp = new Array[Int](intArray.length),
                        lo = 2, mid = 5, hi = 9) should equal(mergedIntArray)
      }
    }

    describe("mergeFunctional") {
      it("returns a single sorted List[Int] when given two serted List[Int]s") {
        val mergedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)
        val intListLeft = List(3,5,6,7,9,10)
        val intListRight = List(1,2,4,5,6,7,7)
        SortUtils.mergeFunctional(intListLeft, intListRight) should equal(mergedIntList)
      }
    }

    describe("partition") {
      it("returns a the index of where the first element would be in the sorted array with all elements less than or equal it to the tleft and all greater to the right") {
        var partitionedArray = Array(5,5,3,4,1,6,2,6,10,7,9,7,7)
        var intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        SortUtils.partition(intArray, 0, intArray.length-1) should equal(7)
        intArray should equal(partitionedArray)
      }
    }
  }
}

class MergeSortTest extends SortTest {

  var sortedIntArray = Array(1,2,3,4,5,5,6,6,7,7,7,9,10)
  var intArray: Array[Int] = _

  val sortedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)

  before {
    intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
  }

  describe("MergeSort") {

    describe("topDownSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        println(intArray.mkString(","))
        MergeSort.topDownSort(intArray) should equal(sortedIntArray)
      }
    }

    describe("bottomUpSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        MergeSort.bottomUpSort(intArray) should equal(sortedIntArray)
      }
    }

    describe("Functional Implementation") {

      describe("topDownSort") {
        it ("returns a sorted List[Int] when given a List[Int] with random values") {
          val intList = List(6,5,3,7,9,10,2,5,6,7,1,4,7)
          MergeSortFunctional.topDownSort(intList) should equal(sortedIntList)
        }
      }
    }
  }
}

class QuickSortTest extends SortTest {

  val sortedIntedArray = Array(1,2,3,4,5,5,6,6,7,7,7,9,10)
  val sortedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)

  describe("QuickSort") {

    describe("sort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        var intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        QuickSort.sort(intArray) should equal(sortedIntedArray)
      }
    }

    describe("Functional Implementation") {

      it("returns a sorted List[Int] when given a List[Int] with random values") {
        val intList = List(6,5,3,7,9,10,2,5,6,7,1,4,7)
        QuickSortFunctional.sort(intList) should equal(sortedIntList)
      }
    }
  }
}


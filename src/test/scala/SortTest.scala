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
  }
  describe("InsertionSortFunctional") {
    describe("sort") {
      it ("returns a sorted List[Int] when given a List[Int] with random values") {
        val intList = List(7,99,1,8,10,3)
        InsertionSortFunctional.sort(intList) should equal (sortedIntList)
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
  object TestObject extends SortUtils
  describe("SortUtils") {
    describe("hmax") {
      it ("returns the maximum h value used by shell sort when given an array size") {
        TestObject.hmax(1) should equal (1)
        TestObject.hmax(10) should equal (4)
        TestObject.hmax(100) should equal (40)
        TestObject.hmax(1000) should equal (364)
      }
    }
    describe("merge") {
      it("returns a single sorted Array[Int] when given two serted Array[Int]s") {
        var mergedIntArray = Array(6,5,2,3,5,6,7,7,9,10,1,4,7)
        var intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        TestObject.merge(input = intArray,
                         tmp = new Array[Int](intArray.length),
                         lo = 2, mid = 5, hi = 9) should equal(mergedIntArray)
      }
    }
    describe("mergeFunctional") {
      it("returns a single sorted List[Int] when given two serted List[Int]s") {
        val mergedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)
        val intListLeft = List(3,5,6,7,9,10)
        val intListRight = List(1,2,4,5,6,7,7)
        TestObject.mergeFunctional(intListLeft, intListRight) should equal(mergedIntList)
      }
    }
    describe("partition") {
      it("returns a the index of where the first element would be in the sorted array with all elements less than or equal it to the tleft and all greater to the right") {
        var partitionedArray = Array(5,5,3,4,1,6,2,6,10,7,9,7,7)
        var intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
        TestObject.partition(intArray, 0, intArray.length-1) should equal(7)
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
        MergeSort.topDownSort(intArray) should equal(sortedIntArray)
      }
    }
    describe("bottomUpSort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        MergeSort.bottomUpSort(intArray) should equal(sortedIntArray)
      }
    }
  }
  describe("MergeSortFunctional") {
    describe("topDownSort") {
      it ("returns a sorted List[Int] when given a List[Int] with random values") {
        val intList = List(6,5,3,7,9,10,2,5,6,7,1,4,7)
        MergeSortFunctional.topDownSort(intList) should equal(sortedIntList)
      }
    }
  }
}

class QuickSortTest extends SortTest {
  var sortedIntArray = Array(1,2,3,4,5,5,6,6,7,7,7,9,10)
  val sortedIntList = List(1,2,3,4,5,5,6,6,7,7,7,9,10)
  var intArray : Array[Int] = _
  before {
    intArray = Array(6,5,3,7,9,10,2,5,6,7,1,4,7)
  }
  describe("QuickSort") {
    describe("sort") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        QuickSort.sort(intArray) should equal(sortedIntArray)
      }
    }
    describe("sort3Part") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        QuickSort.sort3Part(intArray) should equal(sortedIntArray)
      }
    }
    describe("sortCutoff") {
      it("returns a sorted Array[Int] when given an Array[Int] with random values") {
        QuickSort.sortCutoff(3)(intArray) should equal(sortedIntArray)
      }
    }
  }
  describe("QuickSortFunctional") {
    it("returns a sorted List[Int] when given a List[Int] with random values") {
      val intList = List(6,5,3,7,9,10,2,5,6,7,1,4,7)
      QuickSortFunctional.sort(intList) should equal(sortedIntList)
    }
  }
}

import scala.collection.mutable.ArrayBuffer

class PriorityQueueTest extends SortTest {
  var queue : PriorityQueue[String] = _
  before {
    queue  = new PriorityQueue[String]
  }
  describe("PriorityQueue") {
    describe("max") {
      it("returns None when empty") {
        queue.max should equal(None)
      }
      it("returns maximum element when not empty") {
        queue.insertArray(Array("A", "C", "X"))
        queue.max should equal(Some("X"))
      }
    }
    describe("isEmpty") {
      it ("returns true when empty") {
        queue.isEmpty should equal(true)
      }
      it ("returns false when not empty") {
        queue.insert("A"); queue.insert("F"); queue.insert("X")
        queue.isEmpty should equal(false)
      }
    }
    describe("deleteMax") {
      it ("returns None when empty") {
        queue.deleteMax should equal(None)
      }
      it ("returns max element removing it maintaining heap order when not empty and new max is left child") {
        queue.insertArray(Array("A", "C", "X", "T", "M", "V", "H", "U", "I", "R", "K", "L"))
        queue.deleteMax() should equal(Some("X"))
        queue.toArray should equal(Array("V", "U", "L", "T", "R", "C", "H", "A", "I", "M", "K"))
      }
      it ("returns max element removing it maintaining heap order when not empty and new max is right child") {
        queue.insertArray(Array("A", "C", "X", "T", "M", "V", "H", "W", "I", "R", "K", "L"))
        queue.deleteMax() should equal(Some("X"))
        queue.toArray should equal(Array("W", "T", "V", "I", "R", "L", "H", "A", "C", "M", "K"))
      }
      it ("returns all elements ordered from max to min") {
        queue.insertArray(Array("A", "C", "X", "T", "M", "V", "H", "W", "I", "R", "K", "L"))
        val sortedValues = (0 until 12).map((idx) => queue.deleteMax().get)
        sortedValues.toArray should equal(Array("X","W","V","T","R","M","L","K","I","H","C","A"))
      }
    }
    describe("insert") {
      it ("adds elements manitaining heap order") {
        queue.insertArray(Array("A", "C", "X", "T", "M", "V", "H", "U", "I", "R", "K", "L"))
        queue.toArray should equal(Array("X","U","V","T","R","L","H","A","I","M","K", "C"))
      }
    }
  }
}

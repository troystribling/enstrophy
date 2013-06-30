package us.gnos.enstrophy.sort

import scala.reflect.ClassTag
import scala.util.Random

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SortUtils
trait SortUtils {
  def exch[T](array:Array[T], i:Int, j:Int) = {
    val tmp = array(i); array(i) = array(j); array(j) = tmp
  }
  def shuffle[T](input:Array[T]) {
    (input.length-1 to 0 by -1).foreach((i) => this.exch(input, i, Random.nextInt(i+1)))
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SelectionSort
object SelectionSort extends SortUtils {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    input.indices.foreach({(i) =>
      this.exch(input, i, i + this.minIndex(input.drop(i), ordering))
    })
    input
  }
  def minIndex[T](array:Array[T], ordering:Ordering[T])  = {
    (0 /: array.indices) ((minIdx, idx) => if (ordering.gt(array(minIdx), array(idx))) idx else minIdx)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// InsertionSort
object InsertionSort extends SortUtils {
  def hsort[T](input:Array[T], h:Int)(implicit ordering:Ordering[T]) : Array[T] = {
    (h until input.length).foreach({(i) =>
      for (j <- (i until h-1 by -h) if ordering.lt(input(j), input(j-h))) {
        this.exch(input, j, j-h)
      }
    })
    input
  }
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.hsort(input, 1)(ordering)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ShellSort
object ShellSort extends SortUtils {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    var h = this.hmax(input.length)
    while(h >= 1) {
      InsertionSort.hsort(input, h)(ordering)
      h = h/3
    }
    input
  }
  def hmax(n:Int, h:Int = 1) : Int = n/3 > h match {
    case true => this.hmax(n, 3*h+1)
    case false => h
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MergeSort
// arrays can be different sizes but are assumed adjacent first spans lo->mid, second mid+1->h
object MergeSortFunctional extends SortUtils {
  def topDownSort[T](input:List[T])(implicit ordering:Ordering[T]) : List[T]  = {
    val n = input.length/2
    if (n == 0) input
    else {
      val (right, left) = input.splitAt(n)
      this.merge(this.topDownSort(right), this.topDownSort(left))
    }
  }
  def merge[T](left:List[T], right:List[T])(implicit ordering:Ordering[T]) : List[T] = (left, right) match {
    // left is exhausted
    case (Nil, _) => right
    // right is exhausted
    case (_, Nil) => left
    case (leftHead :: leftTail, rightHead :: rightTail) =>
      // left is smaller
      if (ordering.lt(leftHead, rightHead)) leftHead :: this.merge(leftTail, right)(ordering)
      // right is snmaller
      else rightHead :: this.merge(left, rightTail)(ordering)
  }
}

object MergeSort extends SortUtils {
  def topDownSort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.topDownSort(input, new Array[T](input.length), 0, input.length-1, ordering)
  }

  def bottomUpSort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) = {
    val n = input.length
    var tmp = new Array[T](n)
    // log n subsets of size i
    (Iterator.iterate(1)(2*_) takeWhile (_ < n)) foreach ((i) => {
      (Iterator.iterate(0)(_+2*i) takeWhile(_ < n-i)) foreach ((j) => {
        this.merge(input, tmp, j,j+i-1, Math.min(j+2*i-1, n-1))
      })
    })
    input
  }
  def merge[T](input:Array[T], tmp:Array[T], lo:Int, mid:Int, hi:Int)(implicit ordering:Ordering[T]) = {
    var i = lo; var j = mid+1
    (lo to hi).foreach((k) => tmp(k) = input(k))
    (lo to hi).foreach((k) => {
      if (i > mid) {
        // first array is exhausted
        input(k) = tmp(j); j += 1
      } else if (j > hi) {
        // second array is exhausted
        input(k) = tmp(i); i += 1
      } else if (ordering.lt(tmp(j), tmp(i))) {
        // smaller value is second array
        input(k) = tmp(j); j += 1
      } else {
        // smaller value is in first array
        input(k) = tmp(i); i += 1
      }
    })
    input
  }
  private def topDownSort[T](input:Array[T], tmp:Array[T], lo:Int, hi:Int, ordering:Ordering[T]) : Array[T] = hi <= lo match  {
    case true => input
    case false =>
      val mid = lo + (hi - lo)/2
      this.topDownSort(input, tmp, lo, mid, ordering)
      this.topDownSort(input, tmp, mid+1, hi, ordering)
      this.merge(input, tmp, lo, mid, hi)(ordering)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// QuickSort
object QuickSortFunctional {
  def sort[T](input:List[T])(implicit ordering:Ordering[T]) : List[T] = {
    this.rsort(Random.shuffle(input), ordering)
  }
  private def rsort[T](input:List[T], ordering:Ordering[T]) : List[T] = {
    if (input.length <= 1) input
    else {
      val pivot = input.head
      this.rsort(input.filter((i) => ordering.lt(i, pivot)), ordering) ++
        input.filter((i) => ordering.equiv(pivot, i)) ++
        this.rsort(input.filter((i) => ordering.gt(i, pivot)), ordering)
    }
  }
}

object QuickSort extends SortUtils {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.shuffle(input)
    this.sort(input, 0, input.length-1, ordering)
  }
  // use triple partitioning to improve performence when large numbers of elements have the same value
  def sort3Part[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.shuffle(input)
    this.sort3Part(input, 0, input.length-1, ordering)
  }
  // switch to insertion sort for small arrays
  def sortCutoff[T](cutoff:Int)(input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.shuffle(input)
    this.sortCutoff(input, 0, input.length-1, cutoff, ordering)
  }
  def partition[T](input:Array[T], lo:Int, hi:Int)(implicit ordering:Ordering[T]) : Int = {
    var i = lo; var j = hi; val pivot = input(lo)
    // scan from left for values less than pivot and right to left for values greater than pivot
    while(i < j) {
      i = (i to hi).find((k) => ordering.gt(input(k), pivot)) match {
            case Some(idx) => idx
            case None => hi
          }
      j = (j until lo by -1).find((k) => ordering.lteq(input(k), pivot)) match {
            case Some(idx) => idx
            case None => lo
          }
      // place larger values to right of pivot and lower to left
      if (i < j) {
        this.exch(input, i, j)
      }
    }
    this.exch(input, j, lo); j
  }
  private def sort[T](input:Array[T], lo:Int, hi:Int, ordering:Ordering[T]) : Array[T] = {
    if (hi <= lo) input
    else {
      var split = this.partition(input, lo, hi)(ordering)
      this.sort(input, lo, split-1, ordering)
      this.sort(input, split+1, hi, ordering)
    }
  }
  private def sortCutoff[T](input:Array[T], lo:Int, hi:Int, cutoff:Int, ordering:Ordering[T]) : Array[T] = {
    if (hi - cutoff <= lo) this.insertionSort(input, lo, hi)(ordering)
    else {
      var split = this.partition(input, lo, hi)(ordering)
      this.sortCutoff(input, lo, split-1, cutoff, ordering)
      this.sortCutoff(input, split+1, hi, cutoff, ordering)
    }
  }
  private def sort3Part[T](input:Array[T], lo:Int, hi:Int, ordering:Ordering[T]) : Array[T] = {
    if (hi <= lo) input
    else {
      var i = lo+1; var lt = lo; var gt = hi; val pivot = input(lo)
      // scan from left for values less than pivot and right to left for values greater than pivot
      while(i <= gt) {
        val comp = ordering.compare(input(i), pivot)
        if (comp < 0) {
        // element less than pivot
          this.exch(input, lt, i)
          lt += 1; i += 1
        } else if (comp > 0) {
        // element greater than pivot
          this.exch(input, gt, i)
          gt -= 1
        } else {
        // elements are equal
           i += 1
        }
      }
      this.sort3Part(input, lo, lt-1, ordering)
      this.sort3Part(input, gt+1, hi, ordering)
    }
  }
  private def insertionSort[T](input:Array[T], lo:Int, hi:Int)(implicit ordering:Ordering[T]) : Array[T] = {
    (lo to hi).foreach({(i) =>
      for (j <- (i until lo by -1) if ordering.lt(input(j), input(j-1))) {
        this.exch(input, j, j-1)
      }
    })
    input
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Priority Queue
import scala.collection.mutable.ArrayBuffer

class PriorityQueue[T:ClassTag](implicit ordering:Ordering[T]) {
  private var heapVals = new ArrayBuffer[T](0)
  var size = 0
  def max : Option[T] = heapVals.headOption
  def isEmpty = size == 0
  def toArray : Array[T] = heapVals.toArray.take(this.size)
  def deleteMax() : Option[T] = {
    if (this.size > 0) {
      val deletedVal = heapVals(0)
      heapVals(0) = heapVals(this.size-1)
      this.size -= 1
      this.sink()
      Some(deletedVal)
    } else None
  }
  def insertArray(xArray:Array[T]) {
    xArray.foreach((x) => this.insert(x))
  }
  def insert(x:T) {
    if (this.heapVals.length > this.size)
      this.heapVals(this.size) = x
    else
      this.heapVals.append(x)
    this.size += 1
    this.float()
  }
  private def float() {
    // float bottom elemnt up until heap sorted. first find indicies of paraents smaller than last element
    Iterator.iterate(this.size-1)((k) => (k - 1)/2).takeWhile((k) => k >= 1 && this.less((k-1)/2, k))
            .foreach((k) => this.exch(k, (k - 1)/2)) // do element exchanges
  }
  // move k element down until heap sorted
  private def sink(k:Int = 0) {
    // k is index of parent, j is index of left child
    var j = 2*k+1
    // if right child is larger update j to right chaild index
    if ((j < this.size-1) && this.less(j, j+1)) j += 1
    // sink further if parent is less than largest child
    if (this.less(k, j)) {
      // exchange largest child with parent
      this.exch(j, k)
      // go to next level if not leaf
      if (2*j+1 < this.size) this.sink(j)
    }
  }
  private def less(i:Int, j:Int) = {
    this.ordering.lt(this.heapVals(i), this.heapVals(j))
  }
  private def exch(i:Int, j:Int) {
    val tmp = this.heapVals(i); this.heapVals(i) = this.heapVals(j); this.heapVals(j) = tmp;
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// HeapSort
object HeapSort extends SortUtils {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) = {
    val n = input.length
    // put array in heap order
    // for k >= (n-2)/2 - 1 all elements are leaves so sorting can begin at k = (n-2)/2
    ((n-2)/2 to 0 by -1).foreach((i) => {
      this.sink(input, i, n, ordering)
    })
    (n - 1 to 1 by -1).foreach((i) => {
      // move largest element to end of unsorted portion of array
      this.exch(input, 0, i)
      // place unsorted portion of array in heap order
      this.sink(input, 0, i, ordering)
    })
    input
  }
  private def sink[T](input:Array[T], k:Int, n:Int, ordering:Ordering[T]) {
    // k is index of parent, j is index of left child
    var j = 2*k+1
    if (j < n) {
      // if right child is larger update j to right chaild index
      if ((j < n-1) && this.less(input, j, j+1, ordering)) j += 1
      // sink further if parent is less than largest child
      if (this.less(input, k, j, ordering)) {
        // exchange largest child with parent
        this.exch(input, j, k)
        // go to next level if not leaf
        if (2*j+1 < n) this.sink(input, j, n, ordering)
      }
    }
  }
  private def less[T](input:Array[T], i:Int, j:Int, ordering:Ordering[T]) = {
    ordering.lt(input(i), input(j))
  }
}


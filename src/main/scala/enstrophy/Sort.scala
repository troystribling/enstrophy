package us.gnos.enstrophy.sort

import scala.reflect.ClassTag

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SortUtils
object SortUtils {
  def minIndex[T](array:Array[T], ordering:Ordering[T])  = {
    (0 /: array.indices) ((minIdx, idx) => if (ordering.gt(array(minIdx), array(idx))) idx else minIdx)
  }
  def isOrdered[T](array:Array[T])(implicit ordering:Ordering[T]) = {
    (1 until array.length).forall((i) => ordering.lteq(array(i-1),array(i)))
  }
  def exch[T](array:Array[T], i:Int, j:Int) = {
    val tmp = array(i); array(i) = array(j); array(j) = tmp
  }
  def hmax(n:Int, h:Int = 1) : Int = n/3 > h match {
    case true => this.hmax(n, 3*h+1)
    case false => h
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
  def mergeFunctional[T](left:List[T], right:List[T])(implicit ordering:Ordering[T]) : List[T] = (left, right) match {
    // left is exhausted
    case (Nil, _) => right
    // right is exhausted
    case (_, Nil) => left
    case (leftHead :: leftTail, rightHead :: rightTail) =>
      // left is smaller
      if (ordering.lt(leftHead, rightHead)) leftHead :: this.mergeFunctional(leftTail, right)
      // right is snmaller
      else rightHead :: this.mergeFunctional(left, rightTail)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ExchangeSort
object ExchangeSort {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    input.indices.foreach({(i) =>
      SortUtils.exch(input, i, i + SortUtils.minIndex(input.drop(i), ordering))
    })
    input
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// InsertionSort
object InsertionSortFunctional {
  def sort[T:ClassTag](input:List[T])(implicit ordering:Ordering[T]) : List[T] = {
    println(input)
    if (input.isEmpty) Nil
    else this.insert(input.head, this.sort(input.tail), ordering)
  }
  private def insert[T](item:T, input:List[T], ordering:Ordering[T]) : List[T] = {
    println((item, input))
    if (input.isEmpty || ordering.lt(item, input.head)) item :: input
    else input.head :: this.insert(item, input.tail, ordering)
  }
}

object InsertionSort {
  def hsort[T](input:Array[T], h:Int)(implicit ordering:Ordering[T]) : Array[T] = {
    (h until input.length).foreach({(i) =>
      for (j <- (i until h-1 by -h) if ordering.lt(input(j), input(j-h))) {
        SortUtils.exch(input, j, j-h)
      }
    })
    input
  }
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.hsort(input, 1)(ordering)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// InsertionSortWithoutExchanges Problem 2.1.25
object InsertionSortWithoutExchangesFunctional {

}

object InsertionSortWithoutExchages {

}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ShellSort
object ShellSort {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    var h = SortUtils.hmax(input.length)
    while(h >= 1) {
      InsertionSort.hsort(input, h)(ordering)
      h = h/3
    }
    input
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MergeSort
// arrays can be different sizes but are assumed adjacent first spans lo->mid, second mid+1->h
object MergeSortFunctional {
  def topDownSort[T](input:List[T])(implicit ordering:Ordering[T]) : List[T]  = {
    val n = input.length/2
    if (n == 0) input
    else {
      val (right, left) = input.splitAt(n)
      SortUtils.mergeFunctional(this.topDownSort(right), this.topDownSort(left))
    }
  }
}

object MergeSort {
  def topDownSort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.topDownSort(input, new Array[T](input.length), 0, input.length-1, ordering)
  }

  def bottomUpSort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) = {
    val n = input.length
    var tmp = new Array[T](n)
    (Iterator.iterate(1)(2*_) takeWhile (_ < n)) foreach ((i) => {
      (Iterator.iterate(0)(_+2*i) takeWhile(_ < n-i)) foreach ((j) => {
        SortUtils.merge(input, tmp, j,j+i-1, Math.min(j+2*i-1, n-1))
      })
    })
    input
  }

  private def topDownSort[T](input:Array[T], tmp:Array[T], lo:Int, hi:Int, ordering:Ordering[T]) : Array[T] = hi <= lo match  {
    case true => input
    case false =>
      val mid = lo + (hi - lo)/2
      this.topDownSort(input, tmp, lo, mid, ordering)
      this.topDownSort(input, tmp, mid+1, hi, ordering)
      SortUtils.merge(input, tmp, lo, mid, hi)(ordering)
  }
}




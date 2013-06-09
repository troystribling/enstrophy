package us.gnos.enstrophy.sort

import scala.reflect.ClassTag

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SortUtils
object SortUtils {
  def minIndex[T](array:Array[T], ordering:Ordering[T])  = {
    (array.indices :\ 0) ((idx, minIdx) => if (ordering.gt(array(minIdx), array(idx))) idx else minIdx)
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
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// ExchangeSort
object ExchangeSortFunctional {
  def sort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.sort(input, Array[T](), ordering)
  }
  private def sort[T:ClassTag](input:Array[T], output:Array[T], ordering:Ordering[T]) : Array[T]  = input.isEmpty match {
    case true  => output
    case false =>
      val minIndex = SortUtils.minIndex(input, ordering)
      val nextInput = input.splitAt(minIndex)
      this.sort(nextInput._1 ++ nextInput._2.tail, output :+ input(minIndex), ordering)
  }
}

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
  def hsort[T:ClassTag](input:Array[T], h:Int)(implicit ordering:Ordering[T]) : Array[T] = {
    this.hsort(input, Array[T](), h, 1, ordering)
  }
  def sort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.hsort(input, Array[T](), 1, 1, ordering)
  }
  private def hsort[T:ClassTag](input:Array[T], output:Array[T], hin:Int, hout:Int, ordering:Ordering[T]) : Array[T] =  input.isEmpty match {
    case true => output
    case false =>
      val happly = if (hout > hin) 1 else hout
      val nextVal = input.head
      (happly-1 until output.length by hin).find((i) => ordering.lt(nextVal, output(i))) match {
        case Some(minIdx) =>
          val nextOutput = output.splitAt(minIdx)
          val shiftedOutput = this.shiftRight(nextOutput._2, Array[T](), hin)
          this.hsort(input.tail, (nextOutput._1 :+ nextVal) ++ shiftedOutput, hin, happly+1, ordering)
        case None =>
          this.hsort(input.tail, output :+ nextVal, hin, happly+1, ordering)
      }
  }
  private def shiftRight[T:ClassTag](input:Array[T], output:Array[T], h:Int) : Array[T]  = input.length < h match {
    case true =>
      output ++ input
    case false =>
      val slice = input.take(h)
      this.shiftRight(input.drop(h), (output++slice.tail) :+ slice.head, h)
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
object ShellSortFunctional {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.sort(input, SortUtils.hmax(input.length), ordering)
  }
  private def sort[T](input:Array[T], h:Int, ordering:Ordering[T]) : Array[T] = h < 1 match {
    case true => input
    case false => this.sort(InsertionSort.hsort(input, h)(ordering), h/3, ordering)
  }
}

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



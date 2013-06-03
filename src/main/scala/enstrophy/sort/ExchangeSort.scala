package us.gnos.enstrophy.sort

import scala.reflect.ClassTag

object SortUtils {
  def minIndex[T](array:Array[T], ordering:Ordering[T]) = {
    var minIdx = 0
    for (idx <- 0 until array.length) {
      if (ordering.gt(array(minIdx), array(idx)))
        minIdx = idx
    }
    minIdx
  }
  def isOrdered[T](array:Array[T], ordering:Ordering[T]) = {
    var status = true
    for (i <- 1 until array.length) {
      if (ordering.gt(array(i-1),array(i))) {
        status = false
      }
    }
    status
  }
}

object ExchangeSortFunctional {
  def sort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.sort(input, Array[T](), ordering)
  }

  private def sort[T:ClassTag](input:Array[T], output:Array[T], ordering:Ordering[T]) : Array[T]  = input.isEmpty match {
    case true  => output
    case false =>
      val minIndex = SortUtils.minIndex(input, ordering)
      val nextInput = input.splitAt(minIndex)
      this.sort((nextInput._1 ++ nextInput._2.tail).toArray, output :+ input(minIndex), ordering)
  }
}

object ExchangeSort {
  def sort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) {
    def exch(i:Int, j:Int) = {
      val tmp = input(i); input(i) = input(j); input(j) = tmp
    }
    for (idx <- 0 until input.length) {
      val minIndex = SortUtils.minIndex(input.drop(idx), ordering)
      exch(idx, minIndex)
    }
  }
}


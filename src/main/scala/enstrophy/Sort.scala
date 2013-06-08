package us.gnos.enstrophy

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
      this.sort((nextInput._1 ++ nextInput._2.tail).toArray, output :+ input(minIndex), ordering)
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
  def sort[T:ClassTag](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    this.sort(input, Array[T](), ordering)
  }
  private def sort[T:ClassTag](input:Array[T], output:Array[T], ordering:Ordering[T]) : Array[T] =  input.isEmpty match {
    case true => output
    case false =>
      val nextVal = input.head
      output.indices.find((i) => ordering.lt(nextVal, output(i))) match {
        case Some(minIdx) =>
          val nextOutput = output.splitAt(minIdx)
          this.sort(input.tail, (nextOutput._1 :+ nextVal) ++ nextOutput._2, ordering)
        case None =>
          this.sort(input.tail, output :+ nextVal, ordering)
      }
  }
}

object InsertionSort {
  def sort[T](input:Array[T])(implicit ordering:Ordering[T]) : Array[T] = {
    (1 until input.length).foreach({(i) =>
      for (j <- (i until 0 by -1).toList if ordering.lt(input(j), input(j-1)))
        SortUtils.exch(input, j, j-1)
    })
    input
  }
}


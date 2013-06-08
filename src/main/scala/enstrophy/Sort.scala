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

}

object ShellSort {

}



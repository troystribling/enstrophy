package us.gnos.enstrophy

import scala.util.Random
import scala.reflect.ClassTag

import us.gnos.enstrophy.sort._

object Main {
  def main(args: Array[String]) {
    runSorts()
  }
  def timeSortExecution[T:ClassTag](f: => Array[T])(implicit ordering:Ordering[T]) {
    val start = System.currentTimeMillis
    val sort = f
    println("Execution Time: %dms".format(System.currentTimeMillis - start))
    checkSort(sort)(ordering)
  }
  def randomArray(n:Int) = {
    Array.fill(n)(Random.nextInt(n))
  }
  def checkSort[T](array:Array[T])(implicit ordering:Ordering[T]) {
    if (SortUtils.isOrdered(array)(ordering)) println("SORTED") else println("NOT SORTED")
  }
  def runSorts() {
    val elements = 2000
    printf("RUNNING SORTS\n")
    (1 to 5).foreach((i:Int) => {

      var varArray = randomArray(i*elements)
      println("ExchangeSort")
      timeSortExecution(ExchangeSort.sort(varArray))
    })
  }
}

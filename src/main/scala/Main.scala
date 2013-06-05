package gnos.us.enstrophy

import scala.util.Random
import scala.reflect.ClassTag

import us.gnos.enstrophy.sort._

object Main {
  def main(args: Array[String]) {
    runSorts()
  }
  def timeExecution[T:ClassTag](f: => Array[T]) = {
    val start = System.currentTimeMillis
    val sort = f
    (System.currentTimeMillis - start, f)
  }
  def randomArray(n:Int) = {
    Array.fill(n)(Random.nextInt(n))
  }
  def runSorts() {
    val elements = 2000
    printf("RUNNING SORTS\n")
    (1 to 5).foreach((i:Int) => {

      println("NUMBER OF ELEMENTS: %d".format(i*elements))
      val valArray = randomArray(i*elements)
      val (exeFunTime, exeFunSort) = timeExecution({
          ExchangeSortFunctional.sort(valArray)
      })
      checkSort(exeFunSort)
      println("ExchangeSortFunctional: %dms".format(exeFunTime))

      var varArray = randomArray(i*elements)
      val (exeTime, exeSort) = timeExecution({
        ExchangeSort.sort(varArray)
      })
      checkSort(exeSort)
      println("ExchangeSort: %dms".format(exeTime))}
    )
  }
  def checkSort[T](array:Array[T])(implicit ordering:Ordering[T]) {
    if (SortUtils.isOrdered(array)(ordering)) println("SORTED") else println("NOT SORTED")
  }
}

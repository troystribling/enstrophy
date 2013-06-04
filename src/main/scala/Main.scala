package gnos.us.enstrophy

import scala.util.Random

import us.gnos.enstrophy.sort._

object Main {
  def main(args: Array[String]) {
    runSorts()
  }
  def timeExecution(f: => Unit) = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis - start
  }
  def randomArray(n:Int) = {
    Array.fill(n)(Random.nextInt(n))
  }
  def runSorts() {
    val elements = 10
    printf("RUNNING SORTS\n")
    (1 to 1).foreach((i:Int) => {

      //println("NUMBER OF ELEMENTS: %d".format(i*elements))
      //val valArray = randomArray(i*elements)
      //var exeTime = timeExecution({
        //checkSort(valArray)
        //val sort = ExchangeSortFunctional.sort(valArray)
        //checkSort(sort)
      //})
      //println("ExchangeSortFunctional: %dms".format(exeTime))

      var varArray = randomArray(i*elements)
      println(varArray.mkString("\n"))
      val exeTime = timeExecution({
        checkSort(varArray)
        val sort = ExchangeSort.sort(varArray)
        println("DONE")
        println(sort.mkString("\n"))
        checkSort(sort)
      })
      println("ExchangeSort: %dms".format(exeTime))}
    )
  }
  def checkSort[T](array:Array[T])(implicit ordering:Ordering[T]) {
    if (SortUtils.isOrdered(array)(ordering)) println("SORTED") else println("NOT SORTED")
  }
}

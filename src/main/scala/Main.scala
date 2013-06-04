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
  def randomVector(n:Int) = {
    Array.fill(n)(Random.nextInt(100))
  }
  def runSorts() {
    val elements = 2000
    printf("RUNNING SORTS\n")
    (1 to 5).foreach((i:Int) => {
      println("NUMBER OF ELEMENTS: %d".format(i*elements))
      var exeTime = timeExecution({
        val sort = ExchangeSortFunctional.sort(randomVector(i*elements))
      })
      println("ExchangeSortFunctional: %dms".format(exeTime))
      exeTime = timeExecution({
        val sort = ExchangeSort.sort(randomVector(i*elements))
      })
      println("ExchangeSort: %dms".format(exeTime))}
    )
  }
}

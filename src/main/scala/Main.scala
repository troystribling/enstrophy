package us.gnos.enstrophy

import scala.util.Random

import us.gnos.enstrophy.sort._

object SortRunner {

  // utils
  case class Result(sortType:String, arrayType:String, arraySize:Int, time:Long, isOrdered:Boolean)
  case class Timing(sort:Array[Int], time:Long)

  def isOrdered(array:Array[Int]) = {
    (1 until array.length).forall((i) => array(i-1) <= array(i))
  }
  def time(f: => Array[Int]) = {
    val start = System.currentTimeMillis
    val result = f
    Timing(result, System.currentTimeMillis - start)
  }
  // runners
  def stepSort(sortType:String, arrayType:String) : Seq[Result] = {
    (1 to 5).map((i:Int) => {
      runSort(sortType, arrayType, i)
    })
  }
  def runSort(sortType:String, arrayType:String, n:Int) = {
    val array = this.array(arrayType, n)
    val sort = this.sort(sortType)
    val result = this.time(sort(array))
    Result(sortType, arrayType, n, result.time, this.isOrdered(result.sort))
  }
  def prettyPrint(result:Result) {
    println(s"SortType: ${result.sortType}\nArrayType: ${result.arrayType}\nArraySize: ${result.arraySize}\nRunTime: ${result.time} ms\nOrdered: ${result.isOrdered}")
  }
  def prettyPrintStepSort(results:Seq[Result]) {
    println(s"SortType ArrayType ArraySize RunTime Ordered")
    (0 until results.length).foreach(println(_))
  }
  // sorts
  def sort[T](sortType:String) : (Array[Int]) => Array[Int] = sortType match {
    case "ExchangeSort" =>  ExchangeSort.sort[Int]
  }
  // arrays
  def array(arrayType:String, n:Int) : Array[Int] = arrayType match {
    case "Random" => this.randomArray(n)
    case _ => throw new IllegalArgumentException
  }
  def randomArray(n:Int) = {
    Array.fill(n)(Random.nextInt(n))
  }
}

object Main {
  def main(args:Array[String]) {
    args(0) match {
      case "Sort" =>
        if (args(1) == "TimeReport") {
        } else if (args.length < 4) {
          SortRunner.stepSort(args(1), args(2))
        } else {
          SortRunner.prettyPrint(SortRunner.runSort(args(1), args(2), args(3).toInt))
        }
      case "Search" =>
      case "Graph" =>
      case _ => throw new IllegalArgumentException
    }
  }
}

package us.gnos.enstrophy

import scala.util.Random

import us.gnos.enstrophy.sort._

object SortRunner {

  // utils
  case class Result(sortType:String, arrayType:String, arraySize:Int, time:Long, isOrdered:Boolean)
  case class Timing(sort:Seq[Int], time:Long)

  def isOrdered(array:Seq[Int]) = {
    (1 until array.length).forall((i) => array(i-1) <= array(i))
  }
  def time(f: => Seq[Int]) = {
    val start = System.currentTimeMillis
    val result = f
    Timing(result, System.currentTimeMillis - start)
  }
  // runners
  def run(args:Array[String]) {
    if (args(0) == "TimeReport") {
    } else if (args.length < 3) {
      this.prettyPrintStepSort(this.stepSort(args(0), args(1)))
    } else {
      this.prettyPrint(this.runSort(args(0), args(1), args(2).toInt))
    }
  }
  def stepSort(sortType:String, arrayType:String) : Seq[Result] = {
    (1 to 5).map((i) => {
      runSort(sortType, arrayType, Math.pow(10,i).toInt)
    })
  }
  def runSort(sortType:String, arrayType:String, n:Int) = {
    val array = this.array(arrayType, n)
    val sort = this.sort(sortType)
    val result = this.time(sort(array))
    Result(sortType, arrayType, n, result.time, this.isOrdered(result.sort))
  }
  // io
  def prettyPrint(result:Result) {
    println(s"SortType: ${result.sortType}\n"+
            s"ArrayType: ${result.arrayType}\n"+
            s"ArraySize: ${result.arraySize}\n"+
            s"RunTime: ${result.time} ms\n"+
            s"Ordered: ${result.isOrdered}")
  }
  def prettyPrintStepSort(results:Seq[Result]) {
    println("%-25s %-25s %-15s %-15s %-15s".format("SortType","ArrayType","ArraySize", "RunTime (ms)", "Ordered"))
    results.foreach((result) => {
      println("%-25s %-25s %-25d %-25d %-25s".format(
        result.sortType, result.arrayType, result.arraySize, result.time, if (result.isOrdered) "yes" else "no"))
    })
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
      case "Sort" => SortRunner.run(args.tail)
      case "Search" =>
      case "Graph" =>
      case _ => throw new IllegalArgumentException
    }
  }
}

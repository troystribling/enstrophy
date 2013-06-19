package us.gnos.enstrophy

import scala.util.Random

import us.gnos.enstrophy.sort._

object SortRunner {
  def isOrdered(array:Array[Int]) = {
    (1 until array.length).forall((i) => array(i-1) <= array(i))
  }
  def time(f: => Array[Int]) = {
    val start = System.currentTimeMillis
    val result = f
    (result, System.currentTimeMillis - start)
  }
  // runners
  def stepSort(sortType:String, arrayType:String) {
    (1 to 5).foreach((i:Int) => {

    })
  }
  def runSort(sortType:String, arrayType:String, n:Int) {
    val array = this.array(arrayType, n)
    val sort = this.sort(sortType)
    val result = this.time(sort(array))
    println(s"EXECUTION TIME: ${result._2} ms")
    println(s"IS ORDERED: ${this.isOrdered(result._1)}")
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
        if (args.length < 4)
          SortRunner.stepSort(args(1), args(2))
        else
          SortRunner.runSort(args(1), args(2), args(3).toInt)
      case "Search" =>
      case "Graph" =>
      case _ => throw new IllegalArgumentException
    }
  }
}

package us.gnos.enstrophy

import scala.util.Random

import us.gnos.enstrophy.sort._

object SortRunner {

  // params
  val NSTEPS = 5
  val QUICK_SORT_CUTOFFS = List(5, 10, 20, 30, 40, 50, 100, 200)

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
    if (args.length == 1) {
      // run report for all sorts
      this.printCSV(this.reportSort(args(0)), args(0))
    } else if (args(1) == "QuickSortCutoffScan") {
      this.prettyPrintQuickSortCutoffScan(this.quickSortCutoffScan(args(0)), args(0))
    } else {
      val sortTypes = args(1).split(":")
      if (args.length < 3 && sortTypes.length == 1) {
        // run specified sort for range of array sizes
        this.prettyPrintStepSort(this.stepSort(sortTypes(0), args(0)))
      } else if (args.length == 3 && sortTypes.length == 1) {
        // run speficied sort for specified array size
        this.prettyPrint(this.runSort(sortTypes(0), args(0), args(2).toInt))
      } else {
        // compare specified sorts for a range of array sizes
        this.prettyPrintCompareSort(this.compareSort(sortTypes, args(1)), sortTypes, args(1))
      }
    }
  }
  def compareSort(sortTypes:Array[String], arrayType:String) = {
    sortTypes.map(this.stepSort(_, arrayType))
  }
  def stepSort(sortType:String, arrayType:String) : Seq[Result] = {
    (1 to NSTEPS).map((i) => {
      runSort(sortType, arrayType, Math.pow(10,i).toInt)
    })
  }
  def runSort(sortType:String, arrayType:String, n:Int) = {
    val result = if (this.allSorts.contains(sortType)) {
                  var array = this.array(arrayType, n)
                  val sort = this.sort(sortType)
                  this.time(sort(array))
                } else if (this.allFunctionalSorts.contains(sortType)) {
                  val list = this.list(arrayType, n)
                  val sort = this.sortFunctional(sortType)
                  this.time(sort(list))
                } else {
                  throw new IllegalArgumentException("SortType invalid")
                }
    Result(sortType, arrayType, n, result.time, this.isOrdered(result.sort))
  }
  def reportSort(arrayType:String) = {
    this.allSorts.map((sortType) => {
        println(s"SortType: ${sortType}, ArrayType: ${arrayType}")
        this.stepSort(sortType, arrayType)}
    )
  }
  def quickSortCutoffScan(arrayType:String) : Seq[Result] = {
    val arraySize = 1000000
    QUICK_SORT_CUTOFFS.map((cutoff) => {
      var array = this.array(arrayType, arraySize)
      val sort = QuickSort.sortCutoff[Int](cutoff)_
      val result = this.time(sort(array))
      Result("QuickSortCutoff", arrayType, arraySize, result.time, this.isOrdered(result.sort))
    })
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
    println(s"SortType: ${results.head.sortType}")
    println(s"ArrayType: ${results.head.arrayType}")
    println("%-15s %-15s %-15s".format("ArraySize", "RunTime (ms)", "Ordered"))
    results.foreach((result) => {
      println("%-15d %-15d %-15s".format(
        result.arraySize, result.time, if (result.isOrdered) "yes" else "no"))
    })
  }
  def prettyPrintQuickSortCutoffScan(results:Seq[Result], arrayType:String) {
    println(s"SortType: QuickSortCutoff")
    println(s"ArrayType: ${results.head.arrayType}")
    println("%-15s %-15s %-15s %-15s".format("ArraySize", "Cutoff", "RunTime (ms)", "Ordered"))
    (0 until results.length).foreach((i) => {
      val result = results(i)
      println("%-15d %-15d %-15s %-15s".format(
        result.arraySize, QUICK_SORT_CUTOFFS(i), result.time, if (result.isOrdered) "yes" else "no"))
    })
  }
  def prettyPrintCompareSort(results:Seq[Seq[Result]], sortTypes:Array[String], arrayType:String) {
    println(s"ArrayType: ${arrayType}")
    println("%-15s".format("ArraySize") ++ sortTypes.map("%-20s".format(_)))
    var output = (1 to NSTEPS).toArray.map((i) => "%-15d".format(Math.pow(10,0).toInt))
    for(i <- (0 until results.length); j <- (0 until results(i).length)) {
      output(j) = output(j) + "%-20s".format(results(i)(j))
    }
    output.foreach(println(_))
  }
  def printCSV(results:Seq[Seq[Result]], arrayType:String) {
    var csvOutput = (1 to NSTEPS).toArray.map((i) => Math.pow(10,i).toInt.toString)
    for (i <- (0 until results.length); j <- (0 until results(i).length)) {
        csvOutput(j) = csvOutput(j) + "," + results(i)(j).time.toString
    }
    val csvFile = new java.io.PrintWriter("report.csv")
    csvFile.println(s"ArratType: ${arrayType}")
    csvFile.println(this.allSorts.mkString(","))
    csvOutput.foreach(csvFile.println(_))
    csvFile.close()
  }
  // sorts
  def sort(sortType:String) : (Array[Int]) => Array[Int] = sortType match {
    case "SelectionSort" =>  SelectionSort.sort[Int]
    case "InsertionSort" => InsertionSort.sort[Int]
    case "ShellSort" => ShellSort.sort[Int]
    case "MergeSortTopDown" => MergeSort.topDownSort[Int]
    case "MergeSortBottomUp" => MergeSort.bottomUpSort[Int]
    case "QuickSort" => QuickSort.sort[Int]
    case "QuickSort3Part" => QuickSort.sort3Part[Int]
    case "QuickSortCutoff" => QuickSort.sortCutoff[Int](5)_
    case "HeapSort" => HeapSort.sort[Int]
    case _ => throw new IllegalArgumentException("SortType invalid")
  }
  def sortFunctional(sortType:String) : (List[Int]) => List[Int] = sortType match {
    case "InsertionSortFunctional" => InsertionSortFunctional.sort[Int]
    case "MergeSortFunctional" => MergeSortFunctional.topDownSort[Int]
    case "QuickSortFunctional" => QuickSortFunctional.sort[Int]
    case _ => throw new IllegalArgumentException("SortType invalid")
  }
  def allSorts = List("SelectionSort", "InsertionSort","ShellSort", "MergeSortTopDown", "MergeSortBottomUp",
                      "QuickSort", "QuickSort3Part", "QuickSortCutoff", "HeapSort")
  def allFunctionalSorts = List("InsertionSortFunctional", "MergeSortFunctional", "QuickSortFunctional")
  // arrays
  def array(arrayType:String, n:Int) : Array[Int] = arrayType match {
    case "Random" => this.randomArray(n)
    case _ => throw new IllegalArgumentException("ArrayType invalid")
  }
  def allArrays = List("Random")
  def randomArray(n:Int) = {
    Array.fill(n)(Random.nextInt(n))
  }
  // lists
  def list(arrayType:String, n:Int) : List[Int] = arrayType match {
    case "Random" => this.randomList(n)
    case _ => throw new IllegalArgumentException("ArrayType invalid")
  }
  def randomList(n:Int) = {
    List.fill(n)(Random.nextInt(n))
  }
}

object Main {
  val help = s"""
    run Sort ArrayType                      run all SortTypes for multiple sizes of ArrayType
    run Sort ArrayType QuickSortCutoffScan  run QuickSortCutoffScan for multiple cutoffs
    run Sort ArrayType SortType             run SortType for multiple sizes of ArrayType
    run Sort ArrayType SportType Steps      run SortType for ArrayTpe with Size

    ArrayTypes: ${SortRunner.allArrays.mkString(",")}

    SortTypes: ${(SortRunner.allSorts ++ SortRunner.allFunctionalSorts).mkString(",")}
  """
  def main(args:Array[String]) {
    if (args.isEmpty) {
      println(help)
    } else {
      args(0) match {
        case "Sort" => SortRunner.run(args.tail)
        case "Search" =>
        case "Graph" =>
        case _ => throw new IllegalArgumentException("Runner type must be Sort, Search or Graph")
      }
    }
  }
}

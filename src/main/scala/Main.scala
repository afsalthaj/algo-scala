import Algorithms._

import scala.collection.{mutable => m}

/**
  * Created by afsalthaj on 7/10/17.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val result = karatSuba(
      "3141592653589793238462643383279502884197169399375105820974944592",
      "2718281828459045235360287471352662497757247093699959574966967627")
    require(result == BigInt("853906976228112049612749053421339067407668089349316557309830025183339413920507978129391484498545282208081466991074804737627824019484"))
    require(mergeSortAndFindInv(m.MutableList(3, 2, 1, 0, 5, 6, 9, 7, 7, 10, 11, 12, 13, 15, 14, 11, 12, 13, 14, 3)) ==
      (40, List(0, 1, 2, 3, 3, 5, 6, 7, 7, 9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15))
    )
    require(mergeSortAndFindInv(m.MutableList(1)) == (0, List(1)))
    require(mergeSortAndFindInv(m.MutableList()) == (0, List()))
    require(mergeSortAndFindInv(m.MutableList(-1, 1, -2, 0, -3)) == (7, List(-3, -2, -1, 0, 1)))
    require(mergeSortAndFindInv(m.MutableList(5, 4, 3, 2)) == (6, List(2, 3, 4, 5)))
    require(mergeSortAndFindInv(m.MutableList(1, 3, 5, 2, 4, 6)) == (3, List(1, 2, 3, 4, 5, 6)))
    require(mergeSortAndFindInv(m.MutableList(1, 3, 5, 2, 4, 6, 9, 10, 11, 0, 1, 2, 3)) ==
      (36, List(0, 1, 1, 2, 2, 3, 3, 4, 5, 6, 9, 10, 11)))
    require(binarySearch(List(1, 2, 3), 3))
    require(!binarySearch(List(1, 2, 3), 4))
    require(binarySearch((0 to 50000).toList, 25001))
    require(!binarySearch(List(0), 4))
    require(!binarySearch(Nil, 4))
    require(binarySearch(List(-1, 2, 0, 1, 2), -1))
    require(quickSortPivotFirst(Array(3, 8, 2, 5, 1, 4, 7, 6)).toList == List(1, 2, 3, 4, 5, 6, 7, 8))
    require(quickSortPivotFirst(Array(3, 8, 2, 5, 1, 4, 7)).toList == List(1, 2, 3, 4, 5, 7, 8))
    require(quickSortPivotFirst(Array(4, 6, 5)).toList == List(4, 5, 6))
    require(quickSortPivotFirst(Array(3, 8)).toList == List(3, 8))
    require(quickSortPivotFirst(Array(8)).toList == List(8))
    //  require(quickSort(Array()).toList == Nil)
    require(quickSortPivotFirst(Array(1000, 999, 998)).toList == List(998, 999, 1000))
    require(quickSortPivotFirst(Array(1000, 999, 998, 997)).toList == List(997, 998, 999, 1000))
    require(quickSortPivotFirst(Array(999999, 999998, 999997, 999996,
      999995, 999994, 999993, 999992, 999991, 999990, 999989,
      999987, 999986, 999985, 999984, 999983, 999982, 999981, 999980,
      999979, 999978, 999977, 999976, 999975, 999974, 999973, 999972)).toList ==
      List(999972, 999973, 999974, 999975, 999976, 999977, 999978, 999979, 999980, 999981, 999982,
        999983, 999984, 999985, 999986, 999987, 999989, 999990, 999991, 999992, 999993, 999994, 999995,
        999996, 999997, 999998, 999999))
    require(quickSortPivotFirst(Array(3, 8, 2, 5, 1, 4, 7, 6)).toList == List(1, 2, 3, 4, 5, 6, 7, 8))
    require(quickSortPivotFirst(Array(3, 8, 2, 5, 1, 4, 7)).toList == List(1, 2, 3, 4, 5, 7, 8))
    require(quickSortPivotFirst(Array(4, 6, 5)).toList == List(4, 5, 6))
    require(quickSortPivotFirst(Array(3, 8)).toList == List(3, 8))
    require(quickSortPivotFirst(Array(8)).toList == List(8))
    //    require(quickSort(Array()).toList == Nil)
    require(quickSortPivotFirst(Array(1000, 999, 998)).toList == List(998, 999, 1000))
    require(quickSortPivotFirst(Array(1000, 999, 998, 997)).toList == List(997, 998, 999, 1000))
    require(quickSortPivotFirst(Array(999999, 999998, 999997, 999996,
      999995, 999994, 999993, 999992, 999991, 999990, 999989,
      999987, 999986, 999985, 999984, 999983, 999982, 999981, 999980,
      999979, 999978, 999977, 999976, 999975, 999974, 999973, 999972)).toList ==
      List(999972, 999973, 999974, 999975, 999976, 999977, 999978, 999979, 999980, 999981, 999982,
        999983, 999984, 999985, 999986, 999987, 999989, 999990, 999991, 999992, 999993, 999994, 999995,
        999996, 999997, 999998, 999999))
    val line: Array[Int] = scala.io.Source.fromResource("quicksort.txt").getLines().map(_.toInt).to[Array]
    require(quickSortPivotFirst(line).toList == (1 to 10000).toList)
    // quick sort algorithm fails if the input is too higher - because of the stack exception.
    // This happens because, the divide and conquer algorithm doesn't work properly for an already
    // sorted algorithm because the divide provides an imbalanced pair of partitions in terms of the
    // the number of elements.
    val bigArray: Array[Int] = (0 to 60000).reverse.toArray
    require(quickSortPivotFirst(bigArray).toList == (0 to 60000).toList)
    require(quickSortPivotFirst_(bigArray).toList == (0 to 60000).toList)
    println(quickSortPivotFirst(Array(4, 5, 6, 8, 1, 2, 3)).toList)
    println(quickSortPivotFirst_(Array(4, 5, 6, 8, 1, 2, 3)).toList)
  }
}
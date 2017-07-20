import Algorithms._

import scala.collection.{ mutable => m }
/**
  * Created by afsalthaj on 7/10/17.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val result = karatSuba(
      "3141592653589793238462643383279502884197169399375105820974944592",
      "2718281828459045235360287471352662497757247093699959574966967627")
    require(result == BigInt("853906976228112049612749053421339067407668089349316557309830025183339413920507978129391484498545282208081466991074804737627824019484"))
    require(mergeSortAndFindInv(m.MutableList(3,2,1,0,5,6,9,7,7,10,11,12,13,15,14,11,12,13,14,3)) ==
      (40,List(0, 1, 2, 3, 3, 5, 6, 7, 7, 9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15))
    )
    require(mergeSortAndFindInv(m.MutableList(1)) == (0,List(1)))
    require(mergeSortAndFindInv(m.MutableList()) == (0,List()))
    require(mergeSortAndFindInv(m.MutableList(-1, 1, -2, 0, -3)) == (7,List(-3, -2, -1, 0, 1)))
    require(mergeSortAndFindInv(m.MutableList(5,4,3,2)) == (6,List(2, 3, 4, 5)) )
    require(mergeSortAndFindInv(m.MutableList(1,3,5,2,4,6)) == (3,List(1, 2, 3, 4, 5, 6)))
    require(mergeSortAndFindInv(m.MutableList(1,3,5,2,4,6,9,10,11,0,1,2,3)) ==
      (36,List(0, 1, 1, 2, 2, 3, 3, 4, 5, 6, 9, 10, 11)))
    require(binarySearch(List(1, 2, 3), 3))
    require(!binarySearch(List(1, 2, 3), 4))
    require(binarySearch((0 to 50000).toList, 25001))
    require(!binarySearch(List(0), 4))
    require(!binarySearch(Nil, 4))
    require(binarySearch(List(-1, 2, 0, 1, 2), -1))
    val line: m.MutableList[Int] = scala.io.Source.fromFile("/Users/afsalthaj/scalatest").getLines().map(_.toInt).to[m.MutableList]
    println(mergeSortAndFindInv(line))
  }}
import java.math.BigInteger

import MergeSort.mergeSortAndFindInv


/**
  * Created by afsalthaj on 7/10/17.
  */
object AlgoScala {
  def karatsuba(x: String, y: String): BigInt = {
    def go(xx: String, yy: String): BigInt = {
      if (yy.length <=2 || xx.length <= 2) {
        BigInt(xx) * BigInt(yy)
      }
      else {
        val xString = xx.toString.toList
        val yString = yy.toString.toList
        val halfN: Int = Math.max(xString.size, yString.size)/2
        val xSplitt = xString.size - halfN
        val ySplitt = yString.size - halfN
        val a = BigInt(xString.take(xSplitt).mkString)
        val b = BigInt(xString.drop(xSplitt).mkString)
        val c = BigInt(yString.take(ySplitt).mkString)
        val d = BigInt(yString.drop(ySplitt).mkString)
        val ac = go(a.toString, c.toString)
        val bd = go(b.toString, d.toString)
        val abcd = go((a + b).toString, (c + d).toString)
        val adplusbc = abcd - (ac + bd)
        val toBeFilled = BigInt(s"10${List.fill(halfN * 2)(0).mkString}")
        val secondToBeFilled = BigInt(s"10${List.fill(halfN)(0).mkString}")
        (toBeFilled * ac) + (secondToBeFilled * adplusbc) + bd
      }
    }

    go(x, y)
  }

  def main(args: Array[String]): Unit = {
    val result = karatsuba(
      "3141592653589793238462643383279502884197169399375105820974944592",
      "2718281828459045235360287471352662497757247093699959574966967627")
    require(result == BigInt("853906976228112049612749053421339067407668089349316557309830025183339413920507978129391484498545282208081466991074804737627824019484"))
    require(mergeSortAndFindInv(List(3,2,1,0,5,6,9,7,7,10,11,12,13,15,14,11,12,13,14,3)) ==
      (40,List(0, 1, 2, 3, 3, 5, 6, 7, 7, 9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15))
    )
    require(mergeSortAndFindInv(List(1)) == (0,List(1)))
    require(mergeSortAndFindInv(List()) == (0,List()))
    require(mergeSortAndFindInv(List(-1, 1, -2, 0, -3)) == (7,List(-3, -2, -1, 0, 1)))
    require(mergeSortAndFindInv(List(5,4,3,2)) == (6,List(2, 3, 4, 5)) )
    require(mergeSortAndFindInv(List(1,3,5,2,4,6)) == (3,List(1, 2, 3, 4, 5, 6)))
  }}
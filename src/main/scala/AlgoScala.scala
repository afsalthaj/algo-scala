import java.math.BigInteger

/**
  * Created by afsalthaj on 7/10/17.
  */
object AlgoScala {
  /**
    * Karatsuba which has a O(n log n)
    * Formula is derived from the concept
    * ab * ef (Ex: 1234 * 5678 ===> a = 12, b = 34 etc)
    * (10^^(n/2)*a + b) * (10 ^^(n/2)*c + d) ===> to derive
    * (10 ^^ n)*ac + (ad + bc) * (10 ^^ (n/2) + bd
    * And the numbers can be recursively multiplied to have
    * a big O of (n log n) instead of n^^2 etc in usual
    * multiplication algorithms.
    */
  def karatSuba(x: String, y: String): BigInt = {
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

  // To be avoided: use of string? Bit Opertions?
  // Use of BigInt seems to be a bit hacky
  def main(args: Array[String]): Unit = {
    val result = karatSuba("3141592653589793238462643383279502884197169399375105820974944592", "2718281828459045235360287471352662497757247093699959574966967627")
    println (result)
  }}
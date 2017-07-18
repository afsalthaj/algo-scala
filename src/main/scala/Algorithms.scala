/**
  * Created by afsalthaj on 7/16/17.
  */
object Algorithms {
  /**
    * Karatsuba multiplication with lesser runtime (big O) than
    * usual multiplication algorithms
    */
  def karatSuba(x: String, y: String): BigInt = {
    def go(xx: String, yy: String): BigInt = {
      if (yy.length <= 2 || xx.length <= 2) {
        BigInt(xx) * BigInt(yy)
      }
      else {
        val xString = xx.toString.toList
        val yString = yy.toString.toList
        val halfN: Int = Math.max(xString.size, yString.size) / 2
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

  /**
    * For a merge sort, the number of times the recursion can happen, given an input
    * array is equal log n (2) : The number of times n should be divided by 2
    * to reach a number less than 1. The algorithm makes use of merge sort to find
    * the number of inversions in the array. The running time is not n^^2 as in
    * brute force approach of iterating one by one, instead it is n*logn since
    * the running time of a merge sort is n log n.
    */
  def mergeSortAndFindInv(x: List[Int]): (Int, List[Int]) = {
    def inner(a: List[Int], b: List[Int], acc: List[Int], inversions: Int): (Int, List[Int]) = {
      (a, b) match {
        case (Nil, bs) => (inversions, acc ++ b)
        case (as, Nil) => (inversions, acc ++ a)
        case (as, bs) if as.head < bs.head => inner(as.tail, bs, acc ++ List(a.head), inversions)
        // The number of inversion when you had to copy a number from right side to the result
        // is equal to the size of the rest of the elements in the first list from the number
        // that is being compared
        case _ => inner(a, b.tail, acc ++ List(b.head), inversions + a.size)
      }
    }

    if (x.size < 2) (0, x)
    else {
      val firstSection = x.take(x.size / 2)
      //println (firstSection)
      val secondSection = x.drop(firstSection.size)

      firstSection.size match {
        case 1 =>
          val (inv, actualSecondSection) =
            if (secondSection.size > 1) mergeSortAndFindInv(secondSection) else (0, secondSection)
          inner(firstSection, actualSecondSection, Nil, inv)
        case _ =>
          val (inversions1, continueSorting1) = mergeSortAndFindInv(firstSection)
          val (inversions2, continueSorting2) = mergeSortAndFindInv(secondSection)
          inner(continueSorting1, continueSorting2, Nil, inversions1 + inversions2)
      }
    }
  }

  /**
    * According to master theorem
    * a; number of recursions = 1
    * b; shrinkage factor = 2
    * d = 0 (as there are zero steps outside recursion)
    * T(n) <= n log n
    */
  def binarySearch(xx: List[Int], y: Int): Boolean = {
    val firstHalf = xx.take(xx.size / 2)
    val secondHalf = xx.drop(firstHalf.size)
    if (secondHalf.isEmpty)
      false
    else if (y == secondHalf.head)
      true
    else if (y > secondHalf.head)
      binarySearch(xx.tail, y)
    else
      binarySearch(firstHalf, y)
  }
}
import scala.collection.mutable

/**
  * Created by afsalthaj on 7/16/17.
  * Fundamental divide and conquer algorithms that performs better for
  * not-so-obvious reasons. Plus, giving a sense of how easy/difficult
  * it is to implement in a hybrid/mostly-functional language like Scala.
  * The algorithms are implemented based on Stanford's Divide and Conquer,
  * Randomization MOOC. Some of the run time is calculated by making use of
  * Master theorem (Read anywhere).
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
    * the running time of a merge sort is n log n. MutableList is provided to avoid
    * the addition `n` factor in append operations with immutable list during recursions.
    */
  def mergeSortAndFindInv(x: mutable.MutableList[Int]): (BigInt, mutable.MutableList[Int]) = {
    def inner(a: mutable.MutableList[Int], b: mutable.MutableList[Int],
              acc: mutable.MutableList[Int], inversions: BigInt): (BigInt, mutable.MutableList[Int]) = {
      if (a.isEmpty)
        (inversions, acc ++= b)
      else if (b.isEmpty)
        (inversions, acc ++= a)
      else if (a.head < b.head) inner(a.tail, b, acc += a.head, inversions)
      else inner(a, b.tail, acc += b.head, inversions + a.size)
    }

    if (x.size < 2) (0, x)
    else {
      val firstSection = x.take(x.size / 2)
      val secondSection = x.drop(firstSection.size)
      val (inversions1, continueSorting1) = mergeSortAndFindInv(firstSection)
      val (inversions2, continueSorting2) = mergeSortAndFindInv(secondSection)
      inner(continueSorting1, continueSorting2, mutable.MutableList[Int](), inversions1 + inversions2)
    }
  }

  /**
    * According to master theorem
    * a; number of recursions = 1
    * b; shrinkage factor = 2
    * d = 0 (as there are zero steps outside recursion)
    * T(n) <= log n
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

  /**
    * quick sort O(n) is the running time.
    * Mutation process is followed since quick sort
    * is highly oriented to `in-place` approach
    * where memory efficiency is taken into account.
    * Please note that, the `strategy` applied before calling
    * recursion is based on:
    * https://stackoverflow.com/questions/33815273/quicksort-worst-case-results-in-stack-overflow#33816144
    * Please note that the complexity came into picture because of the
    * complexity O(n^^2), for already sorted array
    */
  def quickSort_Pivot_First(inputArray: Array[Int]): Array[Int] = {
    var numberOfComparisons = 0

    def partitionSubroutine(pivotElementIndex: Int, deadEnd: Int): Unit = {

      var i = pivotElementIndex + 1
      val pivotElement = inputArray(pivotElementIndex)
      val j = (pivotElementIndex + 1) to deadEnd

      j.foreach(index => {
        numberOfComparisons += 1
        if (inputArray(index) < pivotElement) {
          if (i != index) {
            val temp = inputArray(i)
            inputArray(i) = inputArray(index)
            inputArray(index) = temp
          }
          i += 1
        }
      })

      val temp = inputArray(pivotElementIndex)
      inputArray(pivotElementIndex) = inputArray(i - 1)
      inputArray(i - 1) = temp

      val leftStrategy = (i - 2) - pivotElementIndex
      val rightStrategy = deadEnd - i

      if (leftStrategy <= rightStrategy) {
        if (pivotElementIndex <= (i - 2)) {
          partitionSubroutine(pivotElementIndex, i - 2)
        }
        if (i <= deadEnd) {
          partitionSubroutine(i, deadEnd)
        }
      }

      else {
        if (i <= deadEnd) {
          partitionSubroutine(i, deadEnd)
        }
        if (pivotElementIndex <= (i - 2)) {
          partitionSubroutine(pivotElementIndex, i - 2)
        }
      }
    }

    partitionSubroutine(0, inputArray.length - 1)
    println(s"The number of comparisons made is $numberOfComparisons")
    inputArray
  }

  /**
    * this quick sort is same as that of before, where the partition
    * call returns the position of `i`, which is then passed again
    * to the subroutine explicitly. The only difference is `easy to understand`
    * the strategy to avoid stack overflow.
    * https://stackoverflow.com/questions/33815273/quicksort-worst-case-results-in-stack-overflow#33816144
    */
  def quickSortPivotFirst_(array: Array[Int]): Array[Int] = {
    var numberOfComparisons = 0

    def quickSortM(A: Array[Int], l: Int, r: Int): Array[Int] = {
      def partitionSubroutine(l: Int, r: Int): Int = {
        var i = l + 1
        ((l + 1) to r).foreach(index => {
          numberOfComparisons += 1
          if (A(index) < A(l)) {
            if (i != index) {
              val temp = A(i)
              A(i) = A(index)
              A(index) = temp
            }
            i += 1
          }
        })

        val temp = A(l)
        A(l) = A(i - 1)
        A(i - 1) = temp
        i - 1
      }

      if (r - l < 1) A
      else {
        val p = partitionSubroutine(l, r)
        if (((p - 1) - l) <= (r - (p + 1))) {
          quickSortM(A, l, p - 1)
          quickSortM(A, p + 1, r)
        }
        else {
          quickSortM(A, p + 1, r)
          quickSortM(A, l, p - 1)
        }
      }
    }

    val result = quickSortM(array, 0, array.length - 1)
    println(s"the number of comparisons made is $numberOfComparisons")
    result
  }
}
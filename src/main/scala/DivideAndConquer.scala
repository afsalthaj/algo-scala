import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/**
  * Created by afsalthaj on 7/16/17.
  * Fundamental divide and conquer algorithms that performs better for
  * not-so-obvious reasons. Plus, giving a sense of how easy/difficult
  * it is to implement in a hybrid/mostly-functional language like Scala.
  * The algorithms are implemented based on the theory sessions in 
  * Stanford's Divide and Conquer, Randomization MOOC. Some of the run time is 
  * calculated by making use of Master theorem (Read anywhere).
  */
object DivideAndConquer {
  val logger: Logger = LoggerFactory.getLogger("Divide and Conquer")

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
  def quickSortPivotFirst(inputArray: Array[Int]): Array[Int] = {
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
    logger.info(s"The number of comparisons made when the pivot element is first is $numberOfComparisons")
    inputArray
  }

  /**
    * this quick sort is same as that of before, where the partition
    * call returns the position of `i`, which is then passed again
    * to the subroutine explicitly. The only difference is `easy to understand`
    * the strategy to avoid stack overflow.
    * https://stackoverflow.com/questions/33815273/quicksort-worst-case-results-in-stack-overflow#33816144
    * Performs better as the number of outside operations are lesser when compared to the first
    * implementation.
    */
  def quickSortPivotFirst_(array: Array[Int]): Array[Int] = {
    var numberOfComparisons = 0

    def quickSortM(A: Array[Int], l: Int, r: Int): Array[Int] = {
      def partitionSubroutine(l: Int, r: Int): Int = {
        numberOfComparisons += r - l

        var i = l + 1
        ((l + 1) to r).foreach(index => {
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
    logger.info(s"the number of comparisons made in another implementation of quick sort" +
      s"where pivot element is first: $numberOfComparisons")
    result
  }

  /**
    * To verify the number of comparisons for a given sequence of pivots,
    * when the pivot element is chosen to be the last element in every recursion.
    * There will be duplicate code, but intention here is not re-using functionalities
    * but performance of an algorithm in different scenarios.
    */
  def quickSortPivotLast(inputArrayLast: Array[Int]): Array[Int] = {
    var numberOfComparisons1 = 0

    def partitionSubroutine(pivotElementIndex: Int, deadEnd: Int): Unit = {
      numberOfComparisons1 += (deadEnd - pivotElementIndex)

      val temp1 = inputArrayLast(pivotElementIndex)
      inputArrayLast(pivotElementIndex) = inputArrayLast(deadEnd)
      inputArrayLast(deadEnd) = temp1

      var i = pivotElementIndex + 1
      val pivotElement = inputArrayLast(pivotElementIndex)
      val j = (pivotElementIndex + 1) to deadEnd

      j.foreach(index => {
        if (inputArrayLast(index) < pivotElement) {
          if (i != index) {
            val temp = inputArrayLast(i)
            inputArrayLast(i) = inputArrayLast(index)
            inputArrayLast(index) = temp
          }
          i += 1
        }
      })

      val temp = inputArrayLast(pivotElementIndex)
      inputArrayLast(pivotElementIndex) = inputArrayLast(i - 1)
      inputArrayLast(i - 1) = temp

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

    partitionSubroutine(0, inputArrayLast.length - 1)
    logger.info(s"The number of comparisons when pivot element is last: $numberOfComparisons1")
    inputArrayLast
  }

  /**
    * Quick sort using median-of-three approach. This is to reduce the number of comparisons
    * and for better performance. Please note the run time of a quick sort algorithm with balanced
    * partition is n log n
    */
  def quickSortPivotMedian(inputArrayLast: Array[Int]): Array[Int] = {
    var numberOfComparisons = 0

    def partitionSubroutine(pivotElementIndex: Int, deadEnd: Int): Unit = {
      numberOfComparisons += (deadEnd - pivotElementIndex)
      val length = (pivotElementIndex to deadEnd).size
      // Well, unfortunately, it is that complex to find a middle element.
      val middleIndex = pivotElementIndex + (deadEnd - pivotElementIndex) / 2
      val medianIndex = if (length >= 2) {
        val firstElement = inputArrayLast(pivotElementIndex)
        val lastElement = inputArrayLast(deadEnd)
        val middleElement = inputArrayLast(middleIndex)
        if (middleElement > firstElement && middleElement < lastElement
          || middleElement < firstElement && middleElement > lastElement)
          middleIndex
        else if (firstElement > middleElement && firstElement < lastElement
          || firstElement < middleElement && firstElement > lastElement)
          pivotElementIndex
        else deadEnd
      }
      else pivotElementIndex

      if (medianIndex != pivotElementIndex) {
        val temp = inputArrayLast(pivotElementIndex)
        inputArrayLast(pivotElementIndex) = inputArrayLast(medianIndex)
        inputArrayLast(medianIndex) = temp
      }

      var i = pivotElementIndex + 1
      val pivotElement = inputArrayLast(pivotElementIndex)
      val j = (pivotElementIndex + 1) to deadEnd

      j.foreach(index => {
        if (inputArrayLast(index) < pivotElement) {
          if (i != index) {
            val temp = inputArrayLast(i)
            inputArrayLast(i) = inputArrayLast(index)
            inputArrayLast(index) = temp
          }
          i += 1
        }
      })

      val temp = inputArrayLast(pivotElementIndex)
      inputArrayLast(pivotElementIndex) = inputArrayLast(i - 1)
      inputArrayLast(i - 1) = temp

      if (pivotElementIndex <= (i - 2)) {
        partitionSubroutine(pivotElementIndex, i - 2)
      }
      if (i <= deadEnd) {
        partitionSubroutine(i, deadEnd)
      }
    }

    partitionSubroutine(0, inputArrayLast.length - 1)
    logger.info(s"The number of comparisons when pivot element is median of three: $numberOfComparisons")
    inputArrayLast
  }
}

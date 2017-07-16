/**
  * Created by afsalthaj on 7/16/17.
  */
object MergeSort {
  /**
    * For a merge sort, the number of times the recursion can happen, given an input
    * array is equal log n (2) : The number of times n should be divided by 2
    * to reach a number less than 1. The algorithm makes use of merge sort to find
    * the number of inversions in the array. The running time is not n^^2 as in
    * brute force approach of iterating one by one, instead it is n*logn since
    * the running time of a merge sort is n log n.
    */
  def mergeSort(x: List[Int]): (Int, List[Int]) = {
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

    if (x.size < 2)
      (0, x)
    else {
      val firstSection = x.take(x.size / 2)
      //println (firstSection)
      val secondSection = x.drop(firstSection.size)

      firstSection.size match {
        case 1 => {
          val (inv, actualSecondSection) =
            if (secondSection.size > 1) mergeSort(secondSection) else (0, secondSection)
          inner(firstSection, actualSecondSection, Nil, inv)
        }
        case _ =>
          val (inversions1, continueSorting1) = mergeSort(firstSection)
          val (inversions2, continueSorting2) = mergeSort(secondSection)
          inner(continueSorting1, continueSorting2, Nil, inversions1 + inversions2)
      }
    }
  }
}
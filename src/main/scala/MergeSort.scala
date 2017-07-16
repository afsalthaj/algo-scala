/**
  * Created by afsalthaj on 7/16/17.
  */
object MergeSort {
  def mergeSort(x: List[Int]): List[Int] = {
    def inner(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = {
      if (a.isEmpty)
        acc ++ b
      else if (b.isEmpty) acc ++ a
      else if (a.head < b.head)
        inner(a.tail, b, acc ++ List(a.head))
      else inner(b, b.tail, acc ++ List(b.head))
    }

    val firstSection = x.take(x.size / 2)
    val secondSection = x.drop(x.size / 2)

    val sortFirst = mergeSort(firstSection)
    val sortSecond = mergeSort(secondSection)
    inner(sortFirst, sortSecond, Nil)

  }
}

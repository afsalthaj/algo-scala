/**
  * Created by afsalthaj on 7/16/17.
  */
object MergeSort {
  def mergeSort(x: List[Int]): List[Int] = {
    def inner(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = {
      (a, b) match {
        case (Nil, bs) => acc ++ b
        case (as, Nil) => acc ++ a
        case (as, bs) if as.head < bs.head => inner(as.tail, bs, acc ++ List(a.head))
        case _ => inner(a, b.tail, acc ++ List(b.head))
      }
    }

   if(x.size < 2)
     x
   else {
    val firstSection = x.take(x.size / 2)
    //println (firstSection)
    val secondSection = x.drop(firstSection.size)

    firstSection.size match {
      case 1 =>
        inner(firstSection,
          if (secondSection.size > 1)
            mergeSort(secondSection) else secondSection, Nil)
      case _ =>
        val continueSorting1 = mergeSort(firstSection)
        val continueSorting2 = mergeSort(secondSection)
        inner(continueSorting1, continueSorting2, Nil)
    }
    }
  }
}
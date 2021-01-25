package io.github.vyunsergey.tinkoff.fintech.common.alg.sort

import scala.collection.mutable

trait BubbleSort extends Sort {
  def sort[A](list: List[A])(implicit C: Sort.Comparator[A]): List[A] = {
    if (list.length <= 1) list
    else {
      val mutableList: mutable.MutableList[A] = mutable.MutableList(list: _*)
      var continue: Boolean = true
      var k = 1
      while (continue) {
        continue = false
        for {
          i <- mutableList.indices.dropRight(k)
        } yield {
          val (a, b) = (mutableList(i), mutableList(i + 1))
          if (C.compare(a, b) > 0) {
            mutableList(i) = b
            mutableList(i + 1) = a
            continue = true
          }
        }
        k += 1
      }
      mutableList.toList
    }
  }
}

object BubbleSort {
  implicit class BubbleSortOps[A](list: List[A]) extends BubbleSort {
    def bubbleSorted(implicit C: Sort.Comparator[A]): List[A] = sort(list)
  }
}

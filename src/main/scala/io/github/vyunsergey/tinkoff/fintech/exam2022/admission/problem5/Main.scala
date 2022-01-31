package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem5

object Main {
  def main(args: Array[String]): Unit = {
    try {
      solve()
    } catch {
      case e: Exception => System.err.println("Error: " + e.getMessage)
    }
  }

  def solve(): Unit = {
    val in: java.util.Scanner = new java.util.Scanner(System.in)
    val n: Int = in.nextLine.toInt
    val numsA: Array[Int] = in.nextLine.split(" ").take(n).map(_.toInt)
    val numsB: Array[Int] = in.nextLine.split(" ").take(n).map(_.toInt)
    System.out.println(frogJump(n, numsA, numsB))
  }

  def frogJump(n: Int, numsA: Array[Int], numsB: Array[Int]): Int = {
    val dp: Array[Int] = Array.fill(n + 1)(Int.MaxValue)
    dp(0) = 0

    (1 to n).foreach { i =>
      if (numsA(i - 1) == i) dp(i) = 1
    }

    def findMin(i: Int): Int = {
      val min = List.range(0, numsA(i - 1) + 1).map { k =>
        dp(i - k + (if (k < i) numsB(i - k - 1) else 0))
      }.min
      if (min == Int.MaxValue) min else min + 1
    }

    (1 to n).foreach { i =>
      dp(i) = findMin(i)
    }
    (1 to n).foreach { i =>
      dp(n - i + 1) = findMin(n - i + 1)
    }

    val res = dp(n)
    if (res == Int.MaxValue) -1 else res
  }
}

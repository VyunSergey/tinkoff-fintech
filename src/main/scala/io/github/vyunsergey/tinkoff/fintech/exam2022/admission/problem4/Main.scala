package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem4

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
    val Array(n, m) = in.nextLine.split(" ").take(2).map(_.toInt)
    System.out.println(knightMove(n, m))
  }

  def knightMove(n: Int, m: Int): Int = {
    val dp: Array[Array[Int]] = Array.fill(n + 1)(Array.fill(m + 1)(0))

    if (n >= 1 && m >= 1) dp(1)(1) = 1
    if (n >= 2 && m >= 3) dp(2)(3) = 1
    if (n >= 3 && m >= 2) dp(3)(2) = 1

    (3 to n).foreach { i =>
      (3 to m).foreach { j =>
        dp(i)(j) = dp(i - 1)(j - 2) + dp(i - 2)(j - 1)
      }
    }

    dp(n)(m)
  }
}

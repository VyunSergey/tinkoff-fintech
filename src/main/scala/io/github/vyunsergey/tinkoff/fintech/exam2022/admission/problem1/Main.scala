package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem1

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
    val Array(a, b, n) = in.nextLine.split(" ").take(3).map(_.toInt)
    if (checkNumbers(a, b, n)) System.out.println("YES")
    else System.out.println("NO")
  }

  def checkNumbers(a: Int, b: Int, n: Int): Boolean = {
    val (x, y) = (a + b, a - b)
    (x > 0) && (y > 0) && (x % 2 == 0) && (y % 2 == 0) && (y / 2 >= n)
  }
}

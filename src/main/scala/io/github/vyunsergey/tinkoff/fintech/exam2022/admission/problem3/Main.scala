package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem3

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
    val nums: Array[BigInt] = in.nextLine.split(" ").take(n).map(BigInt.apply)
    System.out.println(findMinFirst(nums))
  }

  def findMinFirst(nums: Array[BigInt]): BigDecimal = {
    nums.sortBy(-_).foldLeft(BigDecimal(0.0)) { case (res: BigDecimal, a: BigInt) =>
      Math.sqrt((res + BigDecimal(a)).doubleValue)
    }.setScale(0, BigDecimal.RoundingMode.CEILING)
  }
}

package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem1

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
    val Array(n, k) = in.nextLine().split(" ").take(2).map(_.toInt)
    val digits: Array[String] = in.nextLine().split(" ").take(n)

    assert(1 <= n && n <= 1000)
    assert(1 <= k && k <= 10000)

    val result: BigInt = DigitsPaper.computeDifference(k, digits)

    System.out.print(result)
  }

  object DigitsPaper {
    def getDigitsWithoutNine(digits: Array[String]): Array[(Short, Int)] = {
      digits.flatMap(_.reverse.toCharArray.map(x => x.toString.toShort).filterNot(_ == 9).zipWithIndex)
    }

    def takeDigits(n: Int, input: Array[(Short, Int)]): Array[(Short, Int)] = {
      input.sortBy(_._1).sortBy(x => -x._2).take(n)
    }

    def computeDifference(n: Int, digits: Array[String]): BigInt = {
      val digitsWithRank: Array[(Short, Int)] = takeDigits(n, getDigitsWithoutNine(digits))

      digitsWithRank.map { case (digit, rank) =>
        BigInt(9 - digit) * BigInt(Math.pow(10, rank).toLong)
      }.sum
    }

  }
}

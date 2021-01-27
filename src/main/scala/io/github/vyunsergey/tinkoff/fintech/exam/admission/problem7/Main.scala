package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem7

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
    val n: BigInt = BigInt(in.nextLine())
    val Array(a, b, c) = in.nextLine().split(" ").take(3).map(_.toInt)

    assert(1 <= n && n <= BigInt(10).pow(18))
    assert(1 <= a && a <= 100000)
    assert(1 <= b && b <= 100000)
    assert(1 <= c && c <= 100000)

    val result: BigInt = MoneyWallet.countAllAmounts(n, a, b, c)

    System.out.print(result)
  }

  object MoneyWallet {
    def sumAmount(k: BigInt, l: BigInt, m: BigInt, a: BigInt, b: BigInt, c: BigInt): BigInt = {
      1 + k * a + l * b + m * c
    }

    def countAllAmounts(n: BigInt, a: BigInt, b: BigInt, c: BigInt): BigInt = {
      // println(s"n: $n, a: $a, b: $b, c: $c")

      var acc: BigInt = 0

      for {
        k <- BigInt(0) to (n - 1) / a
        l <- BigInt(0) to (n - 1 - k * a) / b
        m <- BigInt(0) to (n - 1 - k * a - l * b) / c
      } yield {
        val sum: BigInt = sumAmount(k, l, m, a, b, c)
        if (sum <= n) acc = acc + 1
        // println(s"k: $k, l: $l, m: $m, acc: $acc, sum: $sum, n: $n")
      }

      acc
    }
  }
}

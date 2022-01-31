package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem2

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
    val Array(n, m) = in.nextLine.split(" ").take(2).map(BigInt.apply)
    System.out.println(fillRectangle(n, m))
  }

  def fillRectangle(n: BigInt, m: BigInt): BigInt = {
    var N = n
    var M = m
    var K = BigInt(0)
    var res = BigInt(0)

    while(N > 0 && M > 0) {
      if (N == M) {
        N = 0
        res += 1
      }
      else if (N == 1) {
        N = 0
        res += M
      }
      else if (M == 1) {
        M = 0
        res += N
      }
      else {
        K = N
        N = K.max(M) - K.min(M)
        M = K.min(M)
        res += 1
      }
    }
    res
  }
}

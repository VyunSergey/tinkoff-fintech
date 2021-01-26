package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem6

import scala.annotation.tailrec

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
    val Array(a, b, p) = in.nextLine().split(" ").take(3).map(_.toInt)

    assert(1 <= a && a <= b && b < p)

    val result: Int = ReversedFermat.reversedSum(p, a, b)

    System.out.print(result)
  }

  object ReversedFermat {
    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt = {
      if (b == 0) a
      else if (a == 0) b
      else gcd(b, a % b)
    }

    def gcd(arr: Array[BigInt]): BigInt = {
      if (arr.length == 0) 0
      else if (arr.length == 1) arr.head
      else {
        arr.tail.foldLeft(arr.head)(gcd)
      }
    }

    def lcm(a: BigInt, b: BigInt): BigInt = {
      a * b / gcd(a, b)
    }

    def lcm(arr: Array[BigInt]): BigInt = {
      if (arr.length == 0) 0
      else if (arr.length == 1) arr.head
      else {
        arr.tail.foldLeft(arr.head)(lcm)
      }
    }

    def powMod(p: Int, a: BigInt, n: Int): BigInt = {
      val step: Int = 10000
      if (n == 0) 1
      else if (n < step) {
        a.pow(n) % p
      }
      else {
        var k: Int = n / step
        val r: Int =  n - k * step
        var tmp: BigInt = a.pow(r) % p
        while (k > 0) {
          tmp = (tmp * (a.pow(step) % p)) % p
          k = k - 1
        }
        tmp
      }
    }

    @tailrec
    def powModRec(p: Int, a: BigInt, n: Int, acc: BigInt = 1): BigInt = {
      val step: Int = 10000
      if (n == 0) acc % p
      else if (n < step) {
        (a.pow(n) % p * acc) % p
      }
      else {
        val tmp: BigInt = a.pow(step) % p
        powModRec(p, a, n - step, (tmp * acc) % p)
      }
    }

    def reversedSum(p: Int, a: Int, b: Int): Int = {
      val numbers: Array[BigInt] = Array.range(a, b + 1).map(BigInt(_))
      val product: BigInt = lcm(numbers)
      val numerator: BigInt = numbers.map(x => product / x).sum
      val multiplier: BigInt = gcd(product, numerator)

      (((numerator / multiplier) % p) * (reverseNumber(p, product / multiplier) % p) % p).toInt
    }

    def reverseNumber(p: Int, a: BigInt): Int = {
      (powMod(p, a % p, p - 2) % p).toInt
    }

    def reversedNumbers(p: Int): Map[Int, Int] = {
      Array.range(1, p).map(x => (x, reverseNumber(p, x))).toMap
    }

    @tailrec
    def reversedNumbersRec(p: Int, n: Int = 1, numbers: Map[Int, Int] = Map.empty[Int, Int]): Map[Int, Int] = {
      if (numbers.size == p -1) numbers
      else {
        val rest: Array[Int] = Array.range(n, p).filterNot(numbers.contains)
        val a: Int = rest.min
        val b: Int = rest.filter(x => a * x % p == 1).head
        reversedNumbersRec(p, a + 1, numbers ++ Map((a, b), (b, a)))
      }
    }
  }
}

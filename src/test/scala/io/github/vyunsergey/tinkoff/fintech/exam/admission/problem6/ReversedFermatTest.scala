package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem6

import io.github.vyunsergey.tinkoff.fintech.exam.admission.problem6.Main.ReversedFermat
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ReversedFermatTest extends AnyFunSuite with Matchers {
  val primes: Array[Int] = Array(2, 3, 5, 7, 11, 13, 17, 19, 23,
    31, 331, 547, 1201, 2029, 101111, 1100101, 16769023)

  test("ReversedFermat method gcd(a, b) should work") {
    primes.map { p =>
      Array.range(2, 1000).map { n =>
        ReversedFermat.gcd(n, 1) shouldBe 1
        ReversedFermat.gcd(n, n) shouldBe n
        ReversedFermat.gcd(p, n) shouldBe (if (n % p == 0) p else 1)
        ReversedFermat.gcd(p, n) shouldBe ReversedFermat.gcd(n, p)
      }
    }
  }

  test("ReversedFermat method gcd(Array) should work") {
    primes.map { p =>
      val arr = Array.range(2, 1000).map(BigInt(_))
      ReversedFermat.gcd(arr) shouldBe 1
      ReversedFermat.gcd(arr.map(_ * p)) shouldBe p
      ReversedFermat.gcd(arr) shouldBe ReversedFermat.gcd(arr.reverse)
    }
  }

  test("ReversedFermat method lcm(a, b) should work") {
    primes.map { p =>
      Array.range(2, 1000).map { n =>
        ReversedFermat.lcm(BigInt(n), BigInt(1)) shouldBe BigInt(n)
        ReversedFermat.lcm(BigInt(n), BigInt(n)) shouldBe BigInt(n)
        ReversedFermat.lcm(BigInt(p), BigInt(n)) shouldBe (if (n % p == 0) BigInt(n) else BigInt(n) * BigInt(p))
        ReversedFermat.lcm(BigInt(p), BigInt(n)) shouldBe ReversedFermat.lcm(BigInt(n), BigInt(p))
      }
    }
  }

  test("ReversedFermat method lcm(Array) should work") {
    primes.map { p =>
      Array.range(2, 10).map { digit =>
        val arr = Array.fill(1000)(digit).map(BigInt(_))
        ReversedFermat.lcm(arr) shouldBe digit
        ReversedFermat.lcm(arr.map(_ * p)) shouldBe BigInt(digit) * BigInt(p)
        ReversedFermat.lcm(arr) shouldBe ReversedFermat.lcm(arr.reverse)
      }
    }
  }

  test("ReversedFermat method reverseNumber should work") {
    primes.map { p =>
      Array(10, 50, 100).map { n =>
        Array.range(1, p).take(Array(n, p).min).map { a =>
          val b = ReversedFermat.reverseNumber(p, a)
          println(s"p: $p, a: $a, b: $b")
          println()
          (a * b) % p shouldBe 1
        }
      }
    }
  }
}

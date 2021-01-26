package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem1

import io.github.vyunsergey.tinkoff.fintech.exam.admission.problem1.Main.DigitsPaper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DigitsPaperTest extends AnyFunSuite with Matchers {
  test("DigitsPaper method getDigitsWithoutNine should work") {
    Array.range(1, 9).map { length =>
      val digits: Array[String] = Array.range(0, 100).map(_ => util.Random.nextInt(Math.pow(10, length).toInt).toString)
      DigitsPaper.getDigitsWithoutNine(digits).map { case (digit, rank) =>
        0 <= digit && digit <= 8 shouldBe true
        0 <= rank && rank <= length shouldBe true
      }
    }
  }

  test("DigitsPaper method takeDigits should work") {
    Array.range(1, 9).map { length =>
      val digits: Array[String] = Array.range(0, 100).map(_ => util.Random.nextInt(Math.pow(10, length).toInt).toString)
      val digitsWithRank: Array[(Short, Int)] = DigitsPaper.getDigitsWithoutNine(digits)
      Array.range(1, 100).map { n =>
        DigitsPaper.takeDigits(n, digitsWithRank).length shouldBe Array(n, digitsWithRank.length).min
      }
    }
  }

  test("DigitsPaper method computeDifference should work") {
    Array.range(1, 9).map { length =>
      Array.range(1, 10).map { digit =>
        val digits: Array[String] = Array.range(0, 100).map(_ => digit.toString * length)
        DigitsPaper.computeDifference(1, digits) shouldBe BigInt(9 - digit) * BigInt(Math.pow(10, length - 1).toLong)
        DigitsPaper.computeDifference(10, digits) shouldBe (10 * BigInt(9 - digit)) * BigInt(Math.pow(10, length - 1).toLong)
        DigitsPaper.computeDifference(100, digits) shouldBe (100 * BigInt(9 - digit)) * BigInt(Math.pow(10, length - 1).toLong)
      }
    }
  }
}

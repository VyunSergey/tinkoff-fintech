package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem2

import io.github.vyunsergey.tinkoff.fintech.exam.admission.problem2.Main.MagicNumber
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MagicNumberTest extends AnyFunSuite with Matchers {
  test("MagicNumber method getNumber should work") {
    List.range(1, 20).map { length =>
      List.range(1, 10).map { digit =>
        MagicNumber.getNumber(length, digit) shouldBe BigInt(digit.toString * length)
      }
    }
  }

  test("MagicNumber method countNumbers with length should work") {
    List.range(1, 20).map { length =>
      List.range(1, 10).map { digit =>
        val left = MagicNumber.getNumber(length, 1)
        val num = MagicNumber.getNumber(length, digit)
        val right = MagicNumber.getNumber(length, 9)
        MagicNumber.countNumbers(length, left, num) shouldBe digit
        MagicNumber.countNumbers(length, num, right) shouldBe 9 - digit + 1
        MagicNumber.countNumbers(length, left, right) shouldBe 9
      }
    }
  }

  test("MagicNumber method countNumbers should work") {
    List.range(1, 20).zip(List.range(20, 1, -1)).map { case (length1, length2) =>
      List.range(1, 10).map { digit =>
        val lengthMin = List(length1, length2).min
        val lengthMax = List(length1, length2).max
        val left = MagicNumber.getNumber(lengthMin, 1)
        val num1 = MagicNumber.getNumber(lengthMin, digit)
        val num2 = MagicNumber.getNumber(lengthMax, digit)
        val right = MagicNumber.getNumber(lengthMax, 9)

        def coefficient1(x: Int, y: Int): Int = if (x == y) 1 else 0
        def coefficient2(x: Int, y: Int): Int = if (x <= y - 1) y - x - 1 else 0
        def coefficient3(x: Int, y: Int): Int = if (x < y) 1 else 0

        def formula(x: Int, a: Int, y: Int, b: Int): Int = {
          coefficient1(x, y) * (b - a + 1) + coefficient2(x, y) * 9 + coefficient3(x, y) * ((9 - a + 1) + b)
        }

        MagicNumber.countNumbers(num1, num2) shouldBe formula(lengthMin, digit, lengthMax, digit)
        MagicNumber.countNumbers(left, num1) shouldBe formula(lengthMin, 1, lengthMin, digit)
        MagicNumber.countNumbers(num1, right) shouldBe formula(lengthMin, digit, lengthMax, 9)
        MagicNumber.countNumbers(left, num2) shouldBe formula(lengthMin, 1, lengthMax, digit)
        MagicNumber.countNumbers(num2, right) shouldBe formula(lengthMax, digit, lengthMax, 9)
        MagicNumber.countNumbers(left, right) shouldBe formula(lengthMin, 1, lengthMax, 9)
      }
    }
  }
}

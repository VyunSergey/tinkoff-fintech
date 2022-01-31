package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem1

import io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem1.Main.checkNumbers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class CheckNumbersTest extends AnyFunSuite with Matchers {
  test("Check Numbers should correctly work") {
    checkNumbers(5, 3, 1) shouldBe true
    checkNumbers(3, 5, 1) shouldBe false
  }

  test("Check Numbers should correctly work with gen") {
    val combinations = Table(
      ("a", "b", "n"),
      (3, 1, 1),
      (2, 0, 1),
      (3, 1, 1),
      (11, 1, 1),
      (33, 7, 1),
      (200000001, 1, 100000000),
      (100000002, 100000000, 1),
      (200000000, 0, 100000000),
      (200000005, 5, 1)
    )
    forAll(combinations) { (a: Int, b: Int, n: Int) =>
      checkNumbers(a, b, n) shouldBe true
    }
  }
}

package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem3

import io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem3.Main.findMinFirst
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class FindMinFirstTest extends AnyFunSuite with Matchers {
  test("Find Min First should correctly work") {
    findMinFirst(Array(0)) shouldBe 0
    findMinFirst(Array(1)) shouldBe 1
    findMinFirst(Array(1, 9)) shouldBe 2
    findMinFirst(Array(3, 4)) shouldBe 3
    findMinFirst(Array(1, 1, 1)) shouldBe 2
    findMinFirst(Array(4, 16, 81)) shouldBe 3
    findMinFirst(Array(1, 1, 1, 1)) shouldBe 2
    findMinFirst(Array(1, 1, 1, 1, 1)) shouldBe 2
  }

  test("Find Min First should correctly work for (n - 1) zeros + 2^n") {
    val combinations = Table("n", List.range(1, 10)
      .map(n => Array.fill(n)(0.0) :+ Math.pow(2, Math.pow(2, n))): _*)

    forAll(combinations) { (arr: Array[Double]) =>
      findMinFirst(arr.map(BigDecimal(_).toBigInt)) shouldBe 2
    }
  }
}

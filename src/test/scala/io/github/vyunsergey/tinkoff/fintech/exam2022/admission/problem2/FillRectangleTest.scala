package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem2

import io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem2.Main.fillRectangle
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class FillRectangleTest extends AnyFunSuite with Matchers {
  test("Fill Rectangle should correctly work") {
    fillRectangle(3, 4) shouldBe 4
    fillRectangle(5, 3) shouldBe 4
    fillRectangle(5, 10) shouldBe 2
  }

  test("Fill Rectangle should correctly work for (n, n + 1)") {
    val combinations = Table(
      "n",
      BigInt(1),
      BigInt(2),
      BigInt(100),
      BigInt(10000),
      BigInt(100000000),
      BigInt("1000000000000000000")
    )
    forAll(combinations) { (n: BigInt) =>
      fillRectangle(n, n + 1) shouldBe (n + 1)
      fillRectangle(n, 2 * n + 1) shouldBe (n + 2)
    }
  }
}

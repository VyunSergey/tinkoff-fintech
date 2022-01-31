package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem4

import io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem4.Main.knightMove
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class KnightMoveTest extends AnyFunSuite with Matchers {
  test("Knight Move should correctly work") {
    knightMove(0, 0) shouldBe 0
    knightMove(1, 1) shouldBe 1
    knightMove(2, 2) shouldBe 0
    knightMove(3, 2) shouldBe 1
    knightMove(2, 3) shouldBe 1
    knightMove(3, 3) shouldBe 0
    knightMove(4, 4) shouldBe 2
    knightMove(5, 5) shouldBe 0
    knightMove(6, 6) shouldBe 0
    knightMove(7, 7) shouldBe 6
  }

  test("Knight Move should correctly work for (n, n)") {
    val combinations = Table("n", List.range(2, 51): _*)

    forAll(combinations) { (n: Int) =>
      knightMove(n, n) shouldBe (2 * knightMove(n - 1, n - 2))
    }
  }
}

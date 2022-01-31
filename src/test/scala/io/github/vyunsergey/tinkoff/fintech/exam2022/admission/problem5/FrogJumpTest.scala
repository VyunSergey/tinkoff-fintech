package io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem5

import io.github.vyunsergey.tinkoff.fintech.exam2022.admission.problem5.Main.frogJump
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FrogJumpTest extends AnyFunSuite with Matchers {
  test("Frog Jump should correctly work") {
    frogJump(3, Array(0, 2, 2), Array(1, 1, 0)) shouldBe 2
    frogJump(2, Array(1, 1), Array(1, 0)) shouldBe -1
    frogJump(10, Array(0, 1, 2, 3, 5, 5, 6, 7, 8, 5), Array(9, 8, 7, 1, 5, 4, 3, 2, 0, 0)) shouldBe 3
  }
}

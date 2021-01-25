package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem2

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
    val Array(a, b) = in.nextLine().split(" ").take(2)

    assert(b.length >= a.length)
    assert(BigInt(b) >= BigInt(a))

    val result: Int = MagicNumber.countNumbers(BigInt(a), BigInt(b))

    System.out.print(result)
  }

  object MagicNumber {
    /**
     * Returns BigInt number with equal digits and specified length
     * @param length the length of magic number
     * @param digit the digit of magic number
     *
     * @example {{{
     * getNumber(1, 1) == 1
     * getNumber(2, 2) == 22
     * getNumber(3, 3) == 333
     * }}}
     */
    def getNumber(length: Int, digit: Int): BigInt = {
      BigInt(digit.toString * length)
    }

    /**
     * Returns number of all magic numbers this specified length
     * that greater or equal to number `a` and less or equal to number `b`
     * @param length the length of each magic number
     * @param left the bottom number
     * @param right the top number
     *
     * @example {{{
     * countNumbers(1, 1, 9) == List(1, 2, 3, 4, 5, 6, 7, 8, 9).count(x => x >= 1 && x <= 9) == 9
     * countNumbers(2, 10, 50) == List(11, 22, 33, 44, 55, 66, 77, 88, 99).count(x => x >= 10 && x <= 50) == 4
     * countNumbers(3, 500, 1000) == List(111, 222, 333, 444, 555, 666, 777, 888, 999).count(x => x >= 500 && x <= 1000) == 5
     * }}}
     */
    def countNumbers(length: Int, left: BigInt, right: BigInt): Int = {
      List.range(1, 10).map(getNumber(length, _)).count(x => x >= left && x <= right)
    }

    /**
     * Returns number of all magic numbers
     * that greater or equal to number `a` and less or equal to number `b`
     * @param left the bottom number
     * @param right the top number
     *
     * @example {{{
     * countNumbers(1, 30) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 22, 33).length == 12
     * countNumbers(10, 50) == List(11, 22, 33, 44).length == 4
     * countNumbers(500, 1000) == List(666, 777, 888, 999).length == 4
     * }}}
     */
    def countNumbers(left: BigInt, right: BigInt): Int = {
      val leftLen: Int = left.toString.length
      val rightLen: Int = right.toString.length

      if (leftLen == rightLen)
        countNumbers(leftLen, left, right)
      else if (leftLen == rightLen + 1)
        countNumbers(leftLen, left, getNumber(leftLen, 9)) +
          countNumbers(rightLen, getNumber(rightLen, 1), right)
      else
        countNumbers(leftLen, left, getNumber(leftLen, 9)) +
          (rightLen - leftLen - 1) * 9 + countNumbers(rightLen, getNumber(rightLen, 1), right)
    }
  }
}

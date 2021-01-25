package io.github.vyunsergey.tinkoff.fintech.common.io.reader

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream

class ReaderTest extends AnyFunSuite with Matchers {
  test("Reader should parse String") {
    val testStr: String = "some test string"

    Console.withIn(new ByteArrayInputStream(testStr.getBytes)) {
      Reader.readAs[String] shouldBe testStr
    }
  }

  test("Reader should parse Byte") {
    val errStr: String = "not a byte"
    val minByte: Byte = Byte.MinValue
    val maxByte: Byte = Byte.MaxValue

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[Byte] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(minByte.toString.getBytes)) {
      Reader.readAs[Byte] shouldBe minByte
    }

    Console.withIn(new ByteArrayInputStream(maxByte.toString.getBytes)) {
      Reader.readAs[Byte] shouldBe maxByte
    }
  }

  test("Reader should parse Boolean") {
    val errStr: String = "not a boolean"
    val trueBoolean: Boolean = true
    val falseBoolean: Boolean = false

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[Boolean] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(trueBoolean.toString.getBytes)) {
      Reader.readAs[Boolean] shouldBe trueBoolean
    }

    Console.withIn(new ByteArrayInputStream(falseBoolean.toString.getBytes)) {
      Reader.readAs[Boolean] shouldBe falseBoolean
    }
  }

  test("Reader should parse Int") {
    val errStr: String = "not an integer"
    val minInt: Int = Int.MinValue
    val maxInt: Int = Int.MaxValue

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[Int] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(minInt.toString.getBytes)) {
      Reader.readAs[Int] shouldBe minInt
    }

    Console.withIn(new ByteArrayInputStream(maxInt.toString.getBytes)) {
      Reader.readAs[Int] shouldBe maxInt
    }
  }

  test("Reader should parse Long") {
    val errStr: String = "not a long"
    val minLong: Long = Long.MinValue
    val maxLong: Long = Long.MaxValue

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[Long] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(minLong.toString.getBytes)) {
      Reader.readAs[Long] shouldBe minLong
    }

    Console.withIn(new ByteArrayInputStream(maxLong.toString.getBytes)) {
      Reader.readAs[Long] shouldBe maxLong
    }
  }

  test("Reader should parse Double") {
    val errStr: String = "not a double"
    val minDouble: Double = Double.MinValue
    val maxDouble: Double = Double.MaxValue

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[Double] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(minDouble.toString.getBytes)) {
      Reader.readAs[Double] shouldBe minDouble
    }

    Console.withIn(new ByteArrayInputStream(maxDouble.toString.getBytes)) {
      Reader.readAs[Double] shouldBe maxDouble
    }
  }

  test("Reader should parse Float") {
    val errStr: String = "not a float"
    val minFloat: Float = Float.MinValue
    val maxFloat: Float = Float.MaxValue

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[Float] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(minFloat.toString.getBytes)) {
      Reader.readAs[Float] shouldBe minFloat
    }

    Console.withIn(new ByteArrayInputStream(maxFloat.toString.getBytes)) {
      Reader.readAs[Float] shouldBe maxFloat
    }
  }

  test("Reader should parse Option[Int]") {
    val errStr: String = "not an integer"
    val minInt: Int = Int.MinValue
    val maxInt: Int = Int.MaxValue

    Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
      Reader.readAs[Option[Int]] shouldBe None
    }

    Console.withIn(new ByteArrayInputStream(minInt.toString.getBytes)) {
      Reader.readAs[Option[Int]] shouldBe Some(minInt)
    }

    Console.withIn(new ByteArrayInputStream(maxInt.toString.getBytes)) {
      Reader.readAs[Option[Int]] shouldBe Some(maxInt)
    }
  }

  test("Reader should parse Option[Long]") {
    val errStr: String = "not a long"
    val minLong: Long = Long.MinValue
    val maxLong: Long = Long.MaxValue

    Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
      Reader.readAs[Option[Long]] shouldBe None
    }

    Console.withIn(new ByteArrayInputStream(minLong.toString.getBytes)) {
      Reader.readAs[Option[Long]] shouldBe Some(minLong)
    }

    Console.withIn(new ByteArrayInputStream(maxLong.toString.getBytes)) {
      Reader.readAs[Option[Long]] shouldBe Some(maxLong)
    }
  }

  test("Reader should parse Option[Double]") {
    val errStr: String = "not a double"
    val minDouble: Double = Double.MinValue
    val maxDouble: Double = Double.MaxValue

    Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
      Reader.readAs[Option[Double]] shouldBe None
    }

    Console.withIn(new ByteArrayInputStream(minDouble.toString.getBytes)) {
      Reader.readAs[Option[Double]] shouldBe Some(minDouble)
    }

    Console.withIn(new ByteArrayInputStream(maxDouble.toString.getBytes)) {
      Reader.readAs[Option[Double]] shouldBe Some(maxDouble)
    }
  }

  test("Reader should parse Option[Float]") {
    val errStr: String = "not a float"
    val minFloat: Float = Float.MinValue
    val maxFloat: Float = Float.MaxValue

    Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
      Reader.readAs[Option[Float]] shouldBe None
    }

    Console.withIn(new ByteArrayInputStream(minFloat.toString.getBytes)) {
      Reader.readAs[Option[Float]] shouldBe Some(minFloat)
    }

    Console.withIn(new ByteArrayInputStream(maxFloat.toString.getBytes)) {
      Reader.readAs[Option[Float]] shouldBe Some(maxFloat)
    }
  }

  test("Reader should parse (Int, Int)") {
    val errStr: String = "not an integer"
    val minInt: Int = Int.MinValue
    val maxInt: Int = Int.MaxValue

    implicit val parser: Reader.Parser[(Int, Int)] = Reader.tuple2Parser(" ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[(Int, Int)] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(s"$minInt $minInt".getBytes)) {
      Reader.readAs[(Int, Int)] shouldBe (minInt, minInt)
    }

    Console.withIn(new ByteArrayInputStream(s"$maxInt $maxInt".getBytes)) {
      Reader.readAs[(Int, Int)] shouldBe (maxInt, maxInt)
    }
  }

  test("Reader should parse (Int, Int, Int)") {
    val errStr: String = "not an integer"
    val minInt: Int = Int.MinValue
    val maxInt: Int = Int.MaxValue

    implicit val parser: Reader.Parser[(Int, Int, Int)] = Reader.tuple3Parser(" ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[(Int, Int, Int)] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(s"$minInt $minInt $minInt".getBytes)) {
      Reader.readAs[(Int, Int, Int)] shouldBe (minInt, minInt, minInt)
    }

    Console.withIn(new ByteArrayInputStream(s"$maxInt $maxInt $maxInt".getBytes)) {
      Reader.readAs[(Int, Int, Int)] shouldBe (maxInt, maxInt, maxInt)
    }
  }

  test("Reader should parse (Int, Int, Int, Int)") {
    val errStr: String = "not an integer"
    val minInt: Int = Int.MinValue
    val maxInt: Int = Int.MaxValue

    implicit val parser: Reader.Parser[(Int, Int, Int, Int)] = Reader.tuple4Parser(" ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[(Int, Int, Int, Int)] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(s"$minInt $minInt $minInt $minInt".getBytes)) {
      Reader.readAs[(Int, Int, Int, Int)] shouldBe (minInt, minInt, minInt, minInt)
    }

    Console.withIn(new ByteArrayInputStream(s"$maxInt $maxInt $maxInt $maxInt".getBytes)) {
      Reader.readAs[(Int, Int, Int, Int)] shouldBe (maxInt, maxInt, maxInt, maxInt)
    }
  }

  test("Reader should parse (Int, Int, Int, Int, Int)") {
    val errStr: String = "not an integer"
    val minInt: Int = Int.MinValue
    val maxInt: Int = Int.MaxValue

    implicit val parser: Reader.Parser[(Int, Int, Int, Int, Int)] = Reader.tuple5Parser(" ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readAs[(Int, Int, Int, Int, Int)] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(s"$minInt $minInt $minInt $minInt $minInt".getBytes)) {
      Reader.readAs[(Int, Int, Int, Int, Int)] shouldBe (minInt, minInt, minInt, minInt, minInt)
    }

    Console.withIn(new ByteArrayInputStream(s"$maxInt $maxInt $maxInt $maxInt $maxInt".getBytes)) {
      Reader.readAs[(Int, Int, Int, Int, Int)] shouldBe (maxInt, maxInt, maxInt, maxInt, maxInt)
    }
  }

  test("Reader readInput should parse Int") {
    val errStr: String = "not an integer"
    val input1: String =
      """1
        |100
        |""".stripMargin
    val input2: String =
      s"""2
         |${Int.MinValue}
         |${Int.MaxValue}
         |""".stripMargin
    val input3: String =
      """3
        |1
        |2
        |3
        |""".stripMargin

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput[Int] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      Reader.readInput[Int].get(0) shouldBe Some((1, List(100)))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      Reader.readInput[Int].get(0) shouldBe Some((2, List(Int.MinValue, Int.MaxValue)))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      Reader.readInput[Int].get(0) shouldBe Some((3, List(1, 2, 3)))
    }
  }

  test("Reader readInput should parse Long") {
    val errStr: String = "not a long"
    val input1: String =
      """1
        |100
        |""".stripMargin
    val input2: String =
      s"""2
         |${Long.MinValue}
         |${Long.MaxValue}
         |""".stripMargin
    val input3: String =
      """3
        |1
        |2
        |3
        |""".stripMargin

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput[Long] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      Reader.readInput[Long].get(0) shouldBe Some((1, List(100L)))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      Reader.readInput[Long].get(0) shouldBe Some((2, List(Long.MinValue, Long.MaxValue)))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      Reader.readInput[Long].get(0) shouldBe Some((3, List(1L, 2L, 3L)))
    }
  }

  test("Reader readInput should parse Double") {
    val errStr: String = "not a double"
    val input1: String =
      """1
        |100.123
        |""".stripMargin
    val input2: String =
      s"""2
         |${Double.MinValue}
         |${Double.MaxValue}
         |""".stripMargin
    val input3: String =
      """3
        |1.1
        |2.2
        |3.3
        |""".stripMargin

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput[Double] shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      Reader.readInput[Double].get(0) shouldBe Some((1, List(100.123)))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      Reader.readInput[Double].get(0) shouldBe Some((2, List(Double.MinValue, Double.MaxValue)))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      Reader.readInput[Double].get(0) shouldBe Some((3, List(1.1, 2.2, 3.3)))
    }
  }

  test("Reader readInput2 should parse Int") {
    val errStr: String = "not an integer"
    val input1: String =
      """1 1
        |-100
        |100
        |""".stripMargin
    val input2: String =
      s"""2 2
         |${Int.MinValue}
         |${Int.MaxValue}
         |${Int.MinValue}
         |${Int.MaxValue}
         |""".stripMargin
    val input3: String =
      """3 3
        |1
        |2
        |3
        |1
        |2
        |3
        |""".stripMargin

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput2[Int](" ") shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      val result: Map[Int, (Int, List[Int])] = Reader.readInput2[Int](" ")
      result.get(0) shouldBe Some((1, List(-100)))
      result.get(1) shouldBe Some((1, List(100)))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      val result: Map[Int, (Int, List[Int])] = Reader.readInput2[Int](" ")
      result.get(0) shouldBe Some((2, List(Int.MinValue, Int.MaxValue)))
      result.get(1) shouldBe Some((2, List(Int.MinValue, Int.MaxValue)))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      val result: Map[Int, (Int, List[Int])] = Reader.readInput2[Int](" ")
      result.get(0) shouldBe Some((3, List(1, 2, 3)))
      result.get(1) shouldBe Some((3, List(1, 2, 3)))
    }
  }

  test("Reader readInput2 should parse Long") {
    val errStr: String = "not a long"
    val input1: String =
      """1 1
        |-100
        |100
        |""".stripMargin
    val input2: String =
      s"""2 2
         |${Long.MinValue}
         |${Long.MaxValue}
         |${Long.MinValue}
         |${Long.MaxValue}
         |""".stripMargin
    val input3: String =
      """3 3
        |1
        |2
        |3
        |1
        |2
        |3
        |""".stripMargin

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput2[Long](" ") shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      val result: Map[Int, (Int, List[Long])] = Reader.readInput2[Long](" ")
      result.get(0) shouldBe Some((1, List(-100)))
      result.get(1) shouldBe Some((1, List(100)))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      val result: Map[Int, (Int, List[Long])] = Reader.readInput2[Long](" ")
      result.get(0) shouldBe Some((2, List(Long.MinValue, Long.MaxValue)))
      result.get(1) shouldBe Some((2, List(Long.MinValue, Long.MaxValue)))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      val result: Map[Int, (Int, List[Long])] = Reader.readInput2[Long](" ")
      result.get(0) shouldBe Some((3, List(1, 2, 3)))
      result.get(1) shouldBe Some((3, List(1, 2, 3)))
    }
  }

  test("Reader readInput2 should parse Double") {
    val errStr: String = "not a double"
    val input1: String =
      """1 1
        |-100.123
        |100.123
        |""".stripMargin
    val input2: String =
      s"""2 2
         |${Double.MinValue}
         |${Double.MaxValue}
         |${Double.MinValue}
         |${Double.MaxValue}
         |""".stripMargin
    val input3: String =
      """3 3
        |1.1
        |2.2
        |3.3
        |1.1
        |2.2
        |3.3
        |""".stripMargin

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput2[Double](" ") shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      val result: Map[Int, (Int, List[Double])] = Reader.readInput2[Double](" ")
      result.get(0) shouldBe Some((1, List(-100.123)))
      result.get(1) shouldBe Some((1, List(100.123)))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      val result: Map[Int, (Int, List[Double])] = Reader.readInput2[Double](" ")
      result.get(0) shouldBe Some((2, List(Double.MinValue, Double.MaxValue)))
      result.get(1) shouldBe Some((2, List(Double.MinValue, Double.MaxValue)))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      val result: Map[Int, (Int, List[Double])] = Reader.readInput2[Double](" ")
      result.get(0) shouldBe Some((3, List(1.1, 2.2, 3.3)))
      result.get(1) shouldBe Some((3, List(1.1, 2.2, 3.3)))
    }
  }

  test("Reader readInput2 should parse (Int, Int)") {
    val errStr: String = "not an integer"
    val input1: String =
      """1 1
        |-100, -100
        |100, 100
        |""".stripMargin
    val input2: String =
      s"""2 2
         |${Int.MinValue}, ${Int.MaxValue}
         |${Int.MaxValue}, ${Int.MinValue}
         |${Int.MinValue}, ${Int.MaxValue}
         |${Int.MaxValue}, ${Int.MinValue}
         |""".stripMargin
    val input3: String =
      """3 3
        |1, 4
        |2, 5
        |3, 6
        |1, 4
        |2, 5
        |3, 6
        |""".stripMargin
    implicit val parser: Reader.Parser[(Int, Int)] = Reader.tuple2Parser(", ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput2[(Int, Int)](" ") shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      val result: Map[Int, (Int, List[(Int, Int)])] = Reader.readInput2[(Int, Int)](" ")
      result.get(0) shouldBe Some((1, List((-100, -100))))
      result.get(1) shouldBe Some((1, List((100, 100))))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      val result: Map[Int, (Int, List[(Int, Int)])] = Reader.readInput2[(Int, Int)](" ")
      result.get(0) shouldBe Some((2, List((Int.MinValue, Int.MaxValue), (Int.MaxValue, Int.MinValue))))
      result.get(1) shouldBe Some((2, List((Int.MinValue, Int.MaxValue), (Int.MaxValue, Int.MinValue))))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      val result: Map[Int, (Int, List[(Int, Int)])] = Reader.readInput2[(Int, Int)](" ")
      result.get(0) shouldBe Some((3, List((1, 4), (2, 5), (3, 6))))
      result.get(1) shouldBe Some((3, List((1, 4), (2, 5), (3, 6))))
    }
  }

  test("Reader readInput2 should parse (Long, Long)") {
    val errStr: String = "not a long"
    val input1: String =
      """1 1
        |-100, -100
        |100, 100
        |""".stripMargin
    val input2: String =
      s"""2 2
         |${Long.MinValue}, ${Long.MaxValue}
         |${Long.MaxValue}, ${Long.MinValue}
         |${Long.MinValue}, ${Long.MaxValue}
         |${Long.MaxValue}, ${Long.MinValue}
         |""".stripMargin
    val input3: String =
      """3 3
        |1, 4
        |2, 5
        |3, 6
        |1, 4
        |2, 5
        |3, 6
        |""".stripMargin
    implicit val parser: Reader.Parser[(Long, Long)] = Reader.tuple2Parser(", ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput2[(Long, Long)](" ") shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      val result: Map[Int, (Int, List[(Long, Long)])] = Reader.readInput2[(Long, Long)](" ")
      result.get(0) shouldBe Some((1, List((-100, -100))))
      result.get(1) shouldBe Some((1, List((100, 100))))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      val result: Map[Int, (Int, List[(Long, Long)])] = Reader.readInput2[(Long, Long)](" ")
      result.get(0) shouldBe Some((2, List((Long.MinValue, Long.MaxValue), (Long.MaxValue, Long.MinValue))))
      result.get(1) shouldBe Some((2, List((Long.MinValue, Long.MaxValue), (Long.MaxValue, Long.MinValue))))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      val result: Map[Int, (Int, List[(Long, Long)])] = Reader.readInput2[(Long, Long)](" ")
      result.get(0) shouldBe Some((3, List((1, 4), (2, 5), (3, 6))))
      result.get(1) shouldBe Some((3, List((1, 4), (2, 5), (3, 6))))
    }
  }

  test("Reader readInput2 should parse (Double, Double)") {
    val errStr: String = "not a double"
    val input1: String =
      """1 1
        |-100.123, -100.123
        |100.123, 100.123
        |""".stripMargin
    val input2: String =
      s"""2 2
         |${Double.MinValue}, ${Double.MaxValue}
         |${Double.MaxValue}, ${Double.MinValue}
         |${Double.MinValue}, ${Double.MaxValue}
         |${Double.MaxValue}, ${Double.MinValue}
         |""".stripMargin
    val input3: String =
      """3 3
        |1.1, 4.4
        |2.2, 5.5
        |3.3, 6.6
        |1.1, 4.4
        |2.2, 5.5
        |3.3, 6.6
        |""".stripMargin
    implicit val parser: Reader.Parser[(Double, Double)] = Reader.tuple2Parser(", ")

    assertThrows[IllegalArgumentException] {
      Console.withIn(new ByteArrayInputStream(errStr.getBytes)) {
        Reader.readInput2[(Double, Double)](" ") shouldBe errStr
      }
    }

    Console.withIn(new ByteArrayInputStream(input1.getBytes)) {
      val result: Map[Int, (Int, List[(Double, Double)])] = Reader.readInput2[(Double, Double)](" ")
      result.get(0) shouldBe Some((1, List((-100.123, -100.123))))
      result.get(1) shouldBe Some((1, List((100.123, 100.123))))
    }

    Console.withIn(new ByteArrayInputStream(input2.getBytes)) {
      val result: Map[Int, (Int, List[(Double, Double)])] = Reader.readInput2[(Double, Double)](" ")
      result.get(0) shouldBe Some((2, List((Double.MinValue, Double.MaxValue), (Double.MaxValue, Double.MinValue))))
      result.get(1) shouldBe Some((2, List((Double.MinValue, Double.MaxValue), (Double.MaxValue, Double.MinValue))))
    }

    Console.withIn(new ByteArrayInputStream(input3.getBytes)) {
      val result: Map[Int, (Int, List[(Double, Double)])] = Reader.readInput2[(Double, Double)](" ")
      result.get(0) shouldBe Some((3, List((1.1, 4.4), (2.2, 5.5), (3.3, 6.6))))
      result.get(1) shouldBe Some((3, List((1.1, 4.4), (2.2, 5.5), (3.3, 6.6))))
    }
  }
}

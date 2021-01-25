package io.github.vyunsergey.tinkoff.fintech.common.alg.sort

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BubbleSortTest extends AnyFunSuite with Matchers {
  import BubbleSort._

  test("BubbleSort should sort String") {
    val list1: List[String] = List(
      "a", "0", "A",
      "b", "1", "B",
      "c", "2", "C",
      "d", "3", "D",
      "e", "4", "E",
      "f", "5", "F",
      "g", "6", "G"
    )
    val list2: List[String] = List.range(1000, 0, -1).map(_.toString)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Byte") {
    val list1: List[Byte] = List(
      Byte.MinValue.toInt, 0, Byte.MaxValue.toInt,
      Byte.MinValue.toInt, 0, Byte.MaxValue.toInt,
      Byte.MinValue.toInt, 0, Byte.MaxValue.toInt
    ).map(_.toByte)
    val list2: List[Byte] = List.range(1000, 0, -1).map(_.toByte)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Boolean") {
    val list1: List[Boolean] = List(
      true, false, true,
      true, false, true,
      true, false, true
    )
    val list2: List[Boolean] = List.range(1000, 0, -1).map(_ % 2 == 0)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Int") {
    val list1: List[Int] = List(
      Int.MinValue, 0, Int.MaxValue,
      Int.MinValue, 0, Int.MaxValue,
      Int.MinValue, 0, Int.MaxValue
    )
    val list2: List[Int] = List.range(1000, 0, -1)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Long") {
    val list1: List[Long] = List(
      Long.MinValue, 0, Long.MaxValue,
      Long.MinValue, 0, Long.MaxValue,
      Long.MinValue, 0, Long.MaxValue
    )
    val list2: List[Long] = List.range(1000, 0, -1).map(_.toLong)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Double") {
    val list1: List[Double] = List(
      Double.MinValue, 0.0, Double.MaxValue,
      Double.MinValue, 0.0, Double.MaxValue,
      Double.MinValue, 0.0, Double.MaxValue
    )
    val list2: List[Double] = List.range(1000, 0, -1).map(_.toDouble)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Float") {
    val list1: List[Float] = List(
      Float.MinValue, 0, Float.MaxValue,
      Float.MinValue, 0, Float.MaxValue,
      Float.MinValue, 0, Float.MaxValue
    )
    val list2: List[Float] = List.range(1000, 0, -1).map(_.toFloat)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[String]") {
    val list1: List[Option[String]] = List(
      None, Some("a"), Some("0"), Some("A"),
      None, Some("b"), Some("1"), Some("B"),
      None, Some("c"), Some("2"), Some("C"),
      None, Some("d"), Some("3"), Some("D"),
      None, Some("e"), Some("4"), Some("E"),
      None, Some("f"), Some("5"), Some("F"),
      None, Some("g"), Some("6"), Some("G")
    )
    val list2: List[Option[String]] = List(None, None) ++ List.range(1000, 0, -1).map(_.toString).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[Byte]") {
    val list1: List[Option[Byte]] = List(
      None, Some(Byte.MinValue), Some(0.toByte), Some(Byte.MaxValue),
      None, Some(Byte.MinValue), Some(0.toByte), Some(Byte.MaxValue),
      None, Some(Byte.MinValue), Some(0.toByte), Some(Byte.MaxValue)
    )
    val list2: List[Option[Byte]] = List(None, None) ++ List.range(1000, 0, -1).map(_.toByte).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[Boolean]") {
    val list1: List[Option[Boolean]] = List(
      None, Some(true), Some(false), Some(true),
      None, Some(true), Some(false), Some(true),
      None, Some(true), Some(false), Some(true)
    )
    val list2: List[Option[Boolean]] = List(None, None) ++ List.range(1000, 0, -1).map(_ % 2 == 0).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[Int]") {
    val list1: List[Option[Int]] = List(
      None, Some(Int.MinValue), Some(0), Some(Int.MaxValue),
      None, Some(Int.MinValue), Some(0), Some(Int.MaxValue),
      None, Some(Int.MinValue), Some(0), Some(Int.MaxValue)
    )
    val list2: List[Option[Int]] = List(None, None) ++ List.range(1000, 0, -1).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[Long]") {
    val list1: List[Option[Long]] = List(
      None, Some(Long.MinValue), Some(0), Some(Long.MaxValue),
      None, Some(Long.MinValue), Some(0), Some(Long.MaxValue),
      None, Some(Long.MinValue), Some(0), Some(Long.MaxValue)
    )
    val list2: List[Option[Long]] = List(None, None) ++ List.range(1000, 0, -1).map(_.toLong).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[Double]") {
    val list1: List[Option[Double]] = List(
      None, Some(Double.MinValue), Some(0.0), Some(Double.MaxValue),
      None, Some(Double.MinValue), Some(0.0), Some(Double.MaxValue),
      None, Some(Double.MinValue), Some(0.0), Some(Double.MaxValue)
    )
    val list2: List[Option[Double]] = List(None, None) ++ List.range(1000, 0, -1).map(_.toDouble).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort Option[Float]") {
    val list1: List[Option[Float]] = List(
      None, Some(Float.MinValue), Some(0), Some(Float.MaxValue),
      None, Some(Float.MinValue), Some(0), Some(Float.MaxValue),
      None, Some(Float.MinValue), Some(0), Some(Float.MaxValue)
    )
    val list2: List[Option[Float]] = List(None, None) ++ List.range(1000, 0, -1).map(_.toFloat).map(Some(_)) ++ List(None, None)

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort (String, String)") {
    val list1: List[(String, String)] = List(
      "a", "0", "A",
      "b", "1", "B",
      "c", "2", "C",
      "d", "3", "D",
      "e", "4", "E",
      "f", "5", "F",
      "g", "6", "G"
    ).map(x => (x, x))
    val list2: List[(String, String)] = List.range(1000, 0, -1).map(_.toString).map(x => (x, x))

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort (Int, Int)") {
    val list1: List[(Int, Int)] = List(
      Int.MinValue, 0, Int.MaxValue,
      Int.MinValue, 0, Int.MaxValue,
      Int.MinValue, 0, Int.MaxValue
    ).map(x => (x, x))
    val list2: List[(Int, Int)] = List.range(1000, 0, -1).map(x => (x, x))

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort (Long, Long)") {
    val list1: List[(Long, Long)] = List(
      Long.MinValue, 0, Long.MaxValue,
      Long.MinValue, 0, Long.MaxValue,
      Long.MinValue, 0, Long.MaxValue
    ).map(x => (x, x))
    val list2: List[(Long, Long)] = List.range(1000, 0, -1).map(_.toLong).map(x => (x, x))

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }

  test("BubbleSort should sort (Double, Double)") {
    val list1: List[(Double, Double)] = List(
      Double.MinValue, 0.0, Double.MaxValue,
      Double.MinValue, 0.0, Double.MaxValue,
      Double.MinValue, 0.0, Double.MaxValue
    ).map(x => (x, x))
    val list2: List[(Double, Double)] = List.range(1000, 0, -1).map(_.toDouble).map(x => (x, x))

    list1.bubbleSorted shouldBe list1.sorted
    list2.bubbleSorted shouldBe list2.sorted
  }
}

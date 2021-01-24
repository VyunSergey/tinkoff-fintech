package io.github.vyunsergey.tinkoff.fintech.common.io.reader

import scala.util.Try

trait Reader {
  import Reader._

  def readInput[A](implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val n: Int = readAs[Int]
    Map(0 -> (n, List.fill(n)(readAs[A])))
  }

  def readInput2[A](implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int) = (readAs[Int], readAs[Int])
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A]))
    )
  }

  def readInput2[A](regex: String)(implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int) = readAs[(Int, Int)](tuple2Parser[Int](regex))
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A]))
    )
  }

  def readInput3[A](implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int, k: Int) = (readAs[Int], readAs[Int], readAs[Int])
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A])),
      2 -> (k, List.fill(k)(readAs[A]))
    )
  }

  def readInput3[A](regex: String)(implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int, k: Int) = readAs[(Int, Int, Int)](tuple3Parser[Int](regex))
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A])),
      2 -> (k, List.fill(k)(readAs[A]))
    )
  }

  def readInput4[A](implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int, k: Int, l: Int) = (readAs[Int], readAs[Int], readAs[Int], readAs[Int])
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A])),
      2 -> (k, List.fill(k)(readAs[A])),
      3 -> (l, List.fill(l)(readAs[A]))
    )
  }

  def readInput4[A](regex: String)(implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int, k: Int, l: Int) = readAs[(Int, Int, Int, Int)](tuple4Parser[Int](regex))
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A])),
      2 -> (k, List.fill(k)(readAs[A])),
      3 -> (l, List.fill(l)(readAs[A]))
    )
  }

  def readInput5[A](implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int, k: Int, l: Int, s: Int) = (readAs[Int], readAs[Int], readAs[Int], readAs[Int], readAs[Int])
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A])),
      2 -> (k, List.fill(k)(readAs[A])),
      3 -> (l, List.fill(l)(readAs[A])),
      4 -> (s, List.fill(s)(readAs[A]))
    )
  }

  def readInput5[A](regex: String)(implicit P: Parser[A]): Map[Int, (Int, List[A])] = {
    val (n: Int, m: Int, k: Int, l: Int, s: Int) = readAs[(Int, Int, Int, Int, Int)](tuple5Parser[Int](regex))
    Map(
      0 -> (n, List.fill(n)(readAs[A])),
      1 -> (m, List.fill(m)(readAs[A])),
      2 -> (k, List.fill(k)(readAs[A])),
      3 -> (l, List.fill(l)(readAs[A])),
      4 -> (s, List.fill(s)(readAs[A]))
    )
  }

  def readLine(): String = {
    Console.in.readLine()
  }

  def readAs[A](implicit P: Parser[A]): A = {
    P.parse(readLine())
  }
}

object Reader extends Reader {
  trait Parser[A] {
    def parse(str: String): A
  }

  implicit val stringParser: Parser[String] = new Parser[String] {
    def parse(str: String): String = str
  }

  implicit val byteParser: Parser[Byte] = new Parser[Byte] {
    def parse(str: String): Byte = str.toByte
  }

  implicit val booleanParser: Parser[Boolean] = new Parser[Boolean] {
    def parse(str: String): Boolean = str.toBoolean
  }

  implicit val intParser: Parser[Int] = new Parser[Int] {
    def parse(str: String): Int = str.toInt
  }

  implicit val longParser: Parser[Long] = new Parser[Long] {
    def parse(str: String): Long = str.toLong
  }

  implicit val doubleParser: Parser[Double] = new Parser[Double] {
    def parse(str: String): Double = str.toDouble
  }

  implicit val floatParser: Parser[Float] = new Parser[Float] {
    def parse(str: String): Float = str.toFloat
  }

  implicit def optionParser[A](implicit P: Parser[A]): Parser[Option[A]] = new Parser[Option[A]] {
    def parse(str: String): Option[A] = Try(P.parse(str)).toOption
  }

  implicit def tuple2Parser[A](regex: String)(implicit P: Parser[A]): Parser[(A, A)] = new Parser[(A, A)] {
    def parse(str: String): (A, A) = str.split(regex).take(2) match {
      case Array(str1, str2) => (P.parse(str1), P.parse(str2))
      case arr => throw new IllegalArgumentException(s"Can`t parse string: '$str' as tuple2. " +
        s"Expected Array(a, b), but got ${arr.mkString("Array(", ", ", ")")}")
    }
  }

  implicit def tuple3Parser[A](regex: String)(implicit P: Parser[A]): Parser[(A, A, A)] = new Parser[(A, A, A)] {
    def parse(str: String): (A, A, A) = str.split(regex).take(3) match {
      case Array(str1, str2, str3) => (P.parse(str1), P.parse(str2), P.parse(str3))
      case arr => throw new IllegalArgumentException(s"Can`t parse string: '$str' as tuple3. " +
        s"Expected Array(a, b, c), but got ${arr.mkString("Array(", ", ", ")")}")
    }
  }

  implicit def tuple4Parser[A](regex: String)(implicit P: Parser[A]): Parser[(A, A, A, A)] = new Parser[(A, A, A, A)] {
    def parse(str: String): (A, A, A, A) = str.split(regex).take(4) match {
      case Array(str1, str2, str3, str4) => (P.parse(str1), P.parse(str2), P.parse(str3), P.parse(str4))
      case arr => throw new IllegalArgumentException(s"Can`t parse string: '$str' as tuple4. " +
        s"Expected Array(a, b, c, d), but got ${arr.mkString("Array(", ", ", ")")}")
    }
  }

  implicit def tuple5Parser[A](regex: String)(implicit P: Parser[A]): Parser[(A, A, A, A, A)] = new Parser[(A, A, A, A, A)] {
    def parse(str: String): (A, A, A, A, A) = str.split(regex).take(5) match {
      case Array(str1, str2, str3, str4, str5) => (P.parse(str1), P.parse(str2), P.parse(str3), P.parse(str4), P.parse(str5))
      case arr => throw new IllegalArgumentException(s"Can`t parse string: '$str' as tuple5. " +
        s"Expected Array(a, b, c, d, e), but got ${arr.mkString("Array(", ", ", ")")}")
    }
  }
}

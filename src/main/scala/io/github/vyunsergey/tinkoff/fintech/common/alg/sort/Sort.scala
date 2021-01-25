package io.github.vyunsergey.tinkoff.fintech.common.alg.sort

trait Sort {
  import Sort._

  def sort[A](list: List[A])(implicit C: Comparator[A]): List[A]
}

object Sort {
  trait Comparator[A] {
    def compare(a: A, b: A): Int
  }

  implicit val stringComparator: Comparator[String] = new Comparator[String] {
    def compare(a: String, b: String): Int = a.compareTo(b)
  }

  implicit val byteComparator: Comparator[Byte] = new Comparator[Byte] {
    def compare(a: Byte, b: Byte): Int =
      if (a < b) -1
      else if (a == b) 0
      else 1
  }

  implicit val booleanComparator: Comparator[Boolean] = new Comparator[Boolean] {
    def compare(a: Boolean, b: Boolean): Int = (a, b) match {
      case (true, true) => 0
      case (true, false) => 1
      case (false, true) => -1
      case (false, false) => 0
    }
  }

  implicit val intComparator: Comparator[Int] = new Comparator[Int] {
    def compare(a: Int, b: Int): Int =
      if (a < b) -1
      else if (a == b) 0
      else 1
  }

  implicit val longComparator: Comparator[Long] = new Comparator[Long] {
    def compare(a: Long, b: Long): Int =
      if (a < b) -1
      else if (a == b) 0
      else 1
  }

  implicit val doubleComparator: Comparator[Double] = new Comparator[Double] {
    def compare(a: Double, b: Double): Int =
      if (a < b) -1
      else if (a == b) 0
      else 1
  }

  implicit val floatComparator: Comparator[Float] = new Comparator[Float] {
    def compare(a: Float, b: Float): Int =
      if (a < b) -1
      else if (a == b) 0
      else 1
  }

  implicit def optionComparator[A](implicit C: Comparator[A]): Comparator[Option[A]] = new Comparator[Option[A]] {
    def compare(a: Option[A], b: Option[A]): Int = (a, b) match {
      case (Some(x), Some(y)) => C.compare(x, y)
      case (Some(_), None) => 1
      case (None, Some(_)) => -1
      case (None, None) => 0
    }
  }

  implicit def tuple2Comparator[A](implicit C: Comparator[A]): Comparator[(A, A)] = new Comparator[(A, A)] {
    def compare(a: (A, A), b: (A, A)): Int = (a, b) match {
      case ((ax, ay), (bx, by)) =>
        if (C.compare(ax, bx) <= 0 && C.compare(ay, by) < 0) -1
        else if (C.compare(ax, bx) < 0 && C.compare(ay, by) <= 0) -1
        else if (C.compare(ax, bx) >= 0 && C.compare(ay, by) > 0) 1
        else if (C.compare(ax, bx) > 0 && C.compare(ay, by) >= 0) 1
        else 0
    }
  }

  implicit def tuple3Comparator[A](implicit C: Comparator[A]): Comparator[(A, A, A)] = new Comparator[(A, A, A)] {
    def compare(a: (A, A, A), b: (A, A, A)): Int = (a, b) match {
      case ((ax, ay, az), (bx, by, bz)) =>
        if (C.compare(ax, bx) <= 0 && C.compare(ay, by) <= 0 && C.compare(az, bz) < 0) -1
        else if (C.compare(ax, bx) <= 0 && C.compare(ay, by) < 0 && C.compare(az, bz) <= 0) -1
        else if (C.compare(ax, bx) < 0 && C.compare(ay, by) <= 0 && C.compare(az, bz) <= 0) -1
        else if (C.compare(ax, bx) >= 0 && C.compare(ay, by) >= 0 && C.compare(az, bz) > 0) 1
        else if (C.compare(ax, bx) >= 0 && C.compare(ay, by) > 0 && C.compare(az, bz) >= 0) 1
        else if (C.compare(ax, bx) > 0 && C.compare(ay, by) >= 0 && C.compare(az, bz) >= 0) 1
        else 0
    }
  }
}

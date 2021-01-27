package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem5

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
    val n: Int = in.nextLine().toInt
    val points: Array[(Double, Double)] = Array.fill(n) {
      in.nextLine().split(" ").take(2).map(_.toDouble) match {
        case Array(x, y) => (x, y)
      }
    }

    assert(3 <= n && n <= 1000)
    points.foreach { case (x, y) =>
      assert(-1000 <= x && x <= 1000)
      assert(-1000 <= y && y <= 1000)
    }

    val result: Double = Polygon.findVerticalSlice(points)

    System.out.print(result)
  }

  object Polygon {
    def distance(p1: (Double, Double), p2: (Double, Double)): Double = {
      Math.sqrt((p1._1 - p2._1) * (p1._1 - p2._1) + (p1._2 - p2._2) * (p1._2 - p2._2))
    }

    def getTriangleSquare(p1: (Double, Double), p2: (Double, Double), p3: (Double, Double)): Double = {
      val a: Double = distance(p1, p2)
      val b: Double = distance(p2, p3)
      val c: Double = distance(p3, p1)
      val p: Double = (a + b + c) / 2

      Math.sqrt(p * (p - a) * (p - b) * (p - c))
    }

    def getLine(p1: (Double, Double),
                p2: (Double, Double)): ((Double, Double), (Double, Double)) = {
      (p1, p2)
    }

    def determinant(ab: (Double, Double),
                    cd: (Double, Double)): Double = {
      ab._1 * cd._2 - ab._2 * cd._1
    }

    def findLinesIntersection(line1: ((Double, Double), (Double, Double)),
                              line2: ((Double, Double), (Double, Double))): Option[(Double, Double)] = {
      val xa: Double = determinant(line1._1, line1._2)
      val xb: Double = determinant((line1._1._1, 1), (line1._2._1, 1))
      val xc: Double = determinant(line2._1, line2._2)
      val xd: Double = determinant((line2._1._1, 1), (line2._2._1, 1))

      val ya: Double = determinant(line1._1, line1._2)
      val yb: Double = determinant((line1._1._2, 1), (line1._2._2, 1))
      val yc: Double = determinant(line2._1, line2._2)
      val yd: Double = determinant((line2._1._2, 1), (line2._2._2, 1))

      val x: Double = determinant((xa, xb), (xc, xd))
      val y: Double = determinant((ya, yb), (yc, yd))
      val norm: Double = determinant((xb, yb), (xd, yd))

      if (norm != 0) Some((x / norm, y / norm))
      else None
    }

    def findClosestPoint(points: Array[(Double, Double)], point: (Double, Double)): (Double, Double) = {
      points.minBy(p => distance(p, point))
    }

    def getSquare(points: Array[(Double, Double)]): Double = {
      val sortedPoints = points.sortBy(p => distance(p, (0.0, 0.0)))
      val Array(p1, p2, p3) = sortedPoints.take(3)
      val triangleSquare = getTriangleSquare(p1, p2, p3)

      println(s"Square Triangle square: $triangleSquare points: ${Array(p1, p2, p3).mkString("Array(", ", ", ")")}")

      val squares = sortedPoints.drop(3).foldLeft((triangleSquare, Array(p1, p2, p3))) { case ((square, arr), point) =>
        val p1 = findClosestPoint(arr, point)
        val p2 = findClosestPoint(arr.filterNot(_ == p1), point)
        val triangleSquare2 = getTriangleSquare(p1, p2, point)

        println(s"Square Triangle square: $triangleSquare2 points: ${Array(p1, p2, point).mkString("Array(", ", ", ")")}")

        (square + triangleSquare2, arr :+ point)
      }
      squares._1
    }

    def getLeftSquareSlice(x: Double, points: Array[(Double, Double)]): Double = {
      val leftPoints = points.filter(_._1 <= x)
      val upPoint = points.maxBy(_._2)
      val upP1 = findClosestPoint(points.filterNot(_ == upPoint), upPoint)
      val upP2 = findClosestPoint(points.filterNot(Array(upPoint, upP1).contains(_)), upPoint)

      val upIntersectionP1 = findLinesIntersection(getLine(upPoint, upP1), getLine((x, 0), (x, 1)))
      val upIntersectionP2 = findLinesIntersection(getLine(upPoint, upP2), getLine((x, 0), (x, 1)))
      val upIntersection = (upIntersectionP1, upIntersectionP2) match {
        case (Some(p1), Some(p2)) => Array(p1, p2).minBy(_._2)
        case (Some(p1), None) => p1
        case (None, Some(p2)) => p2
        case (None, None) => throw new IllegalArgumentException("both Up lines are parallel with Ox!")
      }

      val downPoint = points.minBy(_._2)
      val downP1 = findClosestPoint(points.filterNot(_ == downPoint), downPoint)
      val downP2 = findClosestPoint(points.filterNot(Array(downPoint, downP1).contains(_)), downPoint)

      val downIntersectionP1 = findLinesIntersection(getLine(downPoint, downP1), getLine((x, 0), (x, 1)))
      val downIntersectionP2 = findLinesIntersection(getLine(downPoint, downP2), getLine((x, 0), (x, 1)))
      val downIntersection = (downIntersectionP1, downIntersectionP2) match {
        case (Some(p1), Some(p2)) => Array(p1, p2).minBy(_._2)
        case (Some(p1), None) => p1
        case (None, Some(p2)) => p2
        case (None, None) => throw new IllegalArgumentException("both Down lines are parallel with Ox!")
      }

      getSquare(leftPoints ++ Array(upIntersection, downIntersection))
    }

    @scala.annotation.tailrec
    def binarySearch(x: Double,
                     left: Double,
                     right: Double,
                     points: Array[(Double, Double)],
                     square: Double,
                     squareSlice: (Double, Array[(Double, Double)]) => Double,
                     delta: Double = 0.0000001): Double = {
      val leftSquare: Double = squareSlice(x, points)

      println(s"BinSearch x: $x, left: $left, right: $right, square: $square, squareSlice: $leftSquare")
      println(s"BinSearch points: ${points.mkString("Array(", ", ", ")")}")

      if (Math.abs(leftSquare - square / 2) < delta || Math.abs(right - left) < delta * delta) x
      else if (leftSquare > square / 2) {
        binarySearch((left + x) / 2, left, x, points, square, squareSlice, delta)
      } else {
        binarySearch((x + right) / 2, x, right, points, square, squareSlice, delta)
      }
    }

    def findVerticalSlice(points: Array[(Double, Double)]): Double = {
      val square: Double = getSquare(points)
      val left: Double = points.map(_._1).min
      val right: Double = points.map(_._1).max

      binarySearch(
        x = (left + right) / 2,
        left = left,
        right = right,
        points = points.map(x => (x._1.toDouble, x._2.toDouble)),
        square = square,
        squareSlice = getLeftSquareSlice
      )
    }
  }
}

package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem3

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
    val n = in.nextLine().toInt
    val students: Array[Int] = in.nextLine().split(" ").take(n).map(_.toInt)

    assert(2 <= n && n <= 1000)
    assert(students.length == n)

    val (resultX, resultY): (Int, Int) = StudentsGrowth.replaceStudents(students)

    System.out.print(s"$resultX $resultY")
  }

  object StudentsGrowth {
    def getGrowths(students: Array[Int]): Array[(Int, Int, Boolean)] = {
      students.zipWithIndex.map { case (growth, ind) =>
        (ind + 1, growth, growth % 2 == 0)
      }
    }

    def countEvenGrowth(growths: Array[(Int, Int, Boolean)]): Int = {
      growths.count(_._3)
    }

    def countOddGrowth(growths: Array[(Int, Int, Boolean)]): Int = {
      growths.count(!_._3)
    }

    def countEvenGrowthInEvenPosition(growths: Array[(Int, Int, Boolean)]): Int = {
      growths.count(x => x._3 && x._1 % 2 == 0)
    }

    def countOddGrowthInOddPosition(growths: Array[(Int, Int, Boolean)]): Int = {
      growths.count(x => !x._3 && x._1 % 2 == 1)
    }

    def findStudentEvenGrowthInOddPosition(growths: Array[(Int, Int, Boolean)]): Option[Int] = {
      growths.filter(x => x._3 && x._1 % 2 == 1).map(_._1).headOption
    }

    def findStudentOddGrowthInEventPosition(growths: Array[(Int, Int, Boolean)]): Option[Int] = {
      growths.filter(x => !x._3 && x._1 % 2 == 0).map(_._1).headOption
    }

    def replaceStudents(students: Array[Int]): (Int, Int) = {
      val n: Int = students.length
      val k: Int = n / 2
      val growths: Array[(Int, Int, Boolean)] = getGrowths(students)
      val evenGrowth: Int = countEvenGrowth(growths)
      val evenGrowthInEvenPos: Int = countEvenGrowthInEvenPosition(growths)
      val oddGrowth: Int = countOddGrowth(growths)
      val oddGrowthInOddPos: Int = countOddGrowthInOddPosition(growths)

      if (n % 2 == 0) {
        if (evenGrowth == k && oddGrowth == k
          && evenGrowthInEvenPos == k - 1 && oddGrowthInOddPos == k - 1) {
          (findStudentEvenGrowthInOddPosition(growths).getOrElse(-1),
            findStudentOddGrowthInEventPosition(growths).getOrElse(-1))
        } else (-1, -1)
      } else {
        if (evenGrowth == k && oddGrowth == k + 1
          && evenGrowthInEvenPos == k - 1 && oddGrowthInOddPos == k) {
          (findStudentEvenGrowthInOddPosition(growths).getOrElse(-1),
            findStudentOddGrowthInEventPosition(growths).getOrElse(-1))
        }
        else if (evenGrowth == k + 1 && oddGrowth == k
          && evenGrowthInEvenPos == k && oddGrowthInOddPos == k - 1) {
          (findStudentEvenGrowthInOddPosition(growths).getOrElse(-1),
            findStudentOddGrowthInEventPosition(growths).getOrElse(-1))
        }
        else (-1, -1)
      }
    }
  }
}

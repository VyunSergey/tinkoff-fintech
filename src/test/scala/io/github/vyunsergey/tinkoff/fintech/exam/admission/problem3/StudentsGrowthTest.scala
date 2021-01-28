package io.github.vyunsergey.tinkoff.fintech.exam.admission.problem3

import io.github.vyunsergey.tinkoff.fintech.exam.admission.problem3.Main.StudentsGrowth
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

class StudentsGrowthTest extends AnyFunSuite with Matchers {
  @tailrec
  final def genNumber(f: Int => Boolean, size: Int = 10, num: Int = util.Random.nextInt(10)): Int = {
    if (f(num)) num
    else genNumber(f, size, util.Random.nextInt(size))
  }

  def genNumbersArray(f: Int => Boolean, n: Int, size: Int = 10): Array[Int] = {
    Array.fill(n)(genNumber(f, size))
  }

  def genEven(size: Int = 1000): Int = genNumber(_ % 2 == 0, size)
  def genOdd(size: Int = 1000): Int = genNumber(_ % 2 == 1, size)
  def genArray(n: Int, size: Int = 1000): Array[Int] = genNumbersArray(_ => true, n, size)
  def genEvenArray(n: Int, size: Int = 1000): Array[Int] = genNumbersArray(_ % 2 == 0, n, size)
  def genOddArray(n: Int, size: Int = 1000): Array[Int] = genNumbersArray(_ % 2 == 1, n, size)

  test("StudentsGrowth method getGrowths should work") {
    Array.range(1, 100).map { length =>
      val students: Array[Int] = genArray(length)
      val growths = StudentsGrowth.getGrowths(students)
      growths.length shouldBe length
      growths.foreach { case (_, growth, isEven) =>
        students.contains(growth) shouldBe true
        isEven shouldBe growth % 2 == 0
      }
    }
  }

  test("StudentsGrowth method countEvenGrowth should work") {
    Array.range(1, 100).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents

      StudentsGrowth.countEvenGrowth(StudentsGrowth.getGrowths(evenStudents)) shouldBe length
      StudentsGrowth.countEvenGrowth(StudentsGrowth.getGrowths(oddStudents)) shouldBe 0
      StudentsGrowth.countEvenGrowth(StudentsGrowth.getGrowths(students)) shouldBe length
    }
  }

  test("StudentsGrowth method countOddGrowth should work") {
    Array.range(1, 100).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents

      StudentsGrowth.countOddGrowth(StudentsGrowth.getGrowths(evenStudents)) shouldBe 0
      StudentsGrowth.countOddGrowth(StudentsGrowth.getGrowths(oddStudents)) shouldBe length
      StudentsGrowth.countOddGrowth(StudentsGrowth.getGrowths(students)) shouldBe length
    }
  }

  test("StudentsGrowth method countEvenGrowthInEvenPosition should work") {
    Array.range(1, 100).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents

      StudentsGrowth.countEvenGrowthInEvenPosition(StudentsGrowth.getGrowths(evenStudents)) shouldBe length / 2
      StudentsGrowth.countEvenGrowthInEvenPosition(StudentsGrowth.getGrowths(oddStudents)) shouldBe 0
      StudentsGrowth.countEvenGrowthInEvenPosition(StudentsGrowth.getGrowths(students)) shouldBe length / 2
    }
  }

  test("StudentsGrowth method countOddGrowthInOddPosition should work") {
    Array.range(1, 100).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents

      StudentsGrowth.countOddGrowthInOddPosition(StudentsGrowth.getGrowths(evenStudents)) shouldBe 0
      StudentsGrowth.countOddGrowthInOddPosition(StudentsGrowth.getGrowths(oddStudents)) shouldBe (if (length % 2 == 0) length / 2 else length / 2 + 1)
      StudentsGrowth.countOddGrowthInOddPosition(StudentsGrowth.getGrowths(students)) shouldBe length / 2
    }
  }

  test("StudentsGrowth method findStudentEvenGrowthInOddPosition should work") {
    Array.range(1, 100).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents

      StudentsGrowth.findStudentEvenGrowthInOddPosition(StudentsGrowth.getGrowths(evenStudents)).isDefined shouldBe true
      StudentsGrowth.findStudentEvenGrowthInOddPosition(StudentsGrowth.getGrowths(oddStudents)).isDefined shouldBe false
      StudentsGrowth.findStudentEvenGrowthInOddPosition(StudentsGrowth.getGrowths(students)).isDefined shouldBe true
    }
  }

  test("StudentsGrowth method findStudentOddGrowthInEventPosition should work") {
    Array.range(1, 100).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents

      StudentsGrowth.findStudentOddGrowthInEventPosition(StudentsGrowth.getGrowths(evenStudents)).isDefined shouldBe false
      StudentsGrowth.findStudentOddGrowthInEventPosition(StudentsGrowth.getGrowths(oddStudents)).isDefined shouldBe (if (length > 1) true else false)
      StudentsGrowth.findStudentOddGrowthInEventPosition(StudentsGrowth.getGrowths(students)).isDefined shouldBe true
    }
  }

  test("StudentsGrowth method replaceStudents should work") {
    def check(p: (Int, Int)): Boolean = p match {
      case (x, y) if x > 0 && y > 0 => true
      case _ => false
    }

    def swap(arr: Array[Int]): Array[Int] = {
      val len = arr.length
      val (even, odd) = (Math.abs(genEven(len - 1)), Math.abs(genOdd(len - 1)))
      val (ae, ao) = (arr(even), arr(odd))
      arr.updated(even, ao).updated(odd, ae)
    }

    val students1: Array[Int] = Array(1, 0, 1, 0)
    val students2: Array[Int] = Array(1, 1, 0, 0)
    val students3: Array[Int] = Array(0, 0, 1, 1)
    val students4: Array[Int] = Array(0, 1, 0, 1)

    check(StudentsGrowth.replaceStudents(students1)) shouldBe false
    check(StudentsGrowth.replaceStudents(students2)) shouldBe true
    check(StudentsGrowth.replaceStudents(students3)) shouldBe true
    check(StudentsGrowth.replaceStudents(students4)) shouldBe false

    Array.range(10, 1000).map { length =>
      val evenStudents: Array[Int] = genEvenArray(length)
      val oddStudents: Array[Int] = genOddArray(length)
      val students: Array[Int] = evenStudents ++ oddStudents
      val studentsOrd: Array[Int] = evenStudents.zip(oddStudents).flatMap{ case (x, y) => Array(y, x) }
      val studentsRep: Array[Int] = swap(studentsOrd)

      check(StudentsGrowth.replaceStudents(evenStudents)) shouldBe false
      check(StudentsGrowth.replaceStudents(oddStudents)) shouldBe false
      check(StudentsGrowth.replaceStudents(students)) shouldBe false
      check(StudentsGrowth.replaceStudents(studentsOrd)) shouldBe false
      check(StudentsGrowth.replaceStudents(studentsRep)) shouldBe true
    }
  }
}

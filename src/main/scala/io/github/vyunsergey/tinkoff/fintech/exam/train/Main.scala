package io.github.vyunsergey.tinkoff.fintech.exam.train

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val in: java.util.Scanner = new java.util.Scanner(System.in)
      val a: Int = in.nextInt()
      val b: Int = in.nextInt()
      System.out.print(a + b)
    } catch {
      case e: Exception => System.err.println("Error: " + e.getMessage)
    }
  }
}

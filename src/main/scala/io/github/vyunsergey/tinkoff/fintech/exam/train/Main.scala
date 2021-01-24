package io.github.vyunsergey.tinkoff.fintech.exam.train

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val a = scala.io.StdIn.readInt()
      val b = scala.io.StdIn.readInt()
      System.out.print(a + b)
    } catch {
      case e: Exception => System.err.println("Error: " + e.getMessage);
    }
  }
}

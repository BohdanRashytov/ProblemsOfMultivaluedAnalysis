package pma2

import java.io._
import PMA2.f

object Writer {

  def write(fileName: String, solution: List[(Double, Double)]) = {
    val writer = new FileWriter(s"$fileName.txt")
    val bufferWriter = new BufferedWriter(writer)
    val points = solution.map(p => (p._1, p._2, f(p._1, p._2)))
    bufferWriter.write("(X1, X2, F(X1, X2)) \n")
    points.foreach(p => bufferWriter.write(s"(${p._1}, ${p._2}, ${p._3}) \n"))
    bufferWriter.flush()
  }
}

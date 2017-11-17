package pma3

import java.io._

object Writer {

  def write(fileName: String,
            X: (Double, Double, Double, Double),
            Q: List[List[Double]],
            solution: List[(Double, Double, Double, Double)]) = {
    val writer = new FileWriter(s"$fileName.txt")
    val bufferWriter = new BufferedWriter(writer)

    bufferWriter.write(s"X: $X \n")

    bufferWriter.write("\n")
    bufferWriter.write("Q:  \n")
    Q.foreach(p => bufferWriter.write(s"(${p.apply(0)}, ${p(1)}, ${p(2)}, ${p(3)}) \n"))

    bufferWriter.write("\n")
    bufferWriter.write("Solution:  \n")
    solution.foreach(p => bufferWriter.write(s"(${p._1}, ${p._2}, ${p._3}, ${p._4}) \n"))
    bufferWriter.flush()
  }
}

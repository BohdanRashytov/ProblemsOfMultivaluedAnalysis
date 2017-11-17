package pma3

import Jama.Matrix

object PMA3 {

  def main(args: Array[String]): Unit = {
    val exp1 = new Experiment("Experiment#1-Symmetric")
    exp1.run()

    val exp2 = new Experiment("Experiment#2-X0", X0 = new Matrix(Array(0.1, 1.0, -1.0, 1.0), 4))
    exp2.run()

    val exp3 = new Experiment("Experiment#3-K-B", K1 = 1000.0, B1 = 75.0)
    exp3.run()

    val exp4 = new Experiment("Experiment#4-M", M1 = 0.25, T = 100, d = 25)
    exp4.run()

    val exp5 = new Experiment("Experiment#5-f", f1 = (t: Double) => t / 50.0, d = 25)
    exp5.run()
  }

}

package pma1

import pma1.PMA1._
import java.lang.Math._

class GaussSeidel(X0: (Double, Double), epsilon: Double = 0.001) {
  private var X: List[(Double, Double)] = List(X0)

  private def iteration(): Boolean = {
    val currentX = X.last
    val ff = (t: Double) => f(currentX._1 - t * dfx1(currentX._1, currentX._2), currentX._2 - t * dfx2(currentX._1, currentX._2))
    val t = new GoldenSection(ff, -10.0, 10.0).solution()
    val nextX = (currentX._1 - t * dfx1(currentX._1, currentX._2), currentX._2 - t * dfx2(currentX._1, currentX._2))
    X = X ::: List(nextX)
    continue(nextX, epsilon)
  }

  private def continue(x: (Double, Double), epsilon: Double) = sqrt(abs(dfx1(x._1, x._2)) + abs(dfx2(x._1, x._2))) > epsilon

  def run() = {
    while (iteration()) {}
    X
  }
}

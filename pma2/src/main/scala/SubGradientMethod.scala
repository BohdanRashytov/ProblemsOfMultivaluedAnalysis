package pma2

import java.lang.Math._
import PMA2._

class SubGradientMethod(X0: (Double, Double), stepRule: StepRule, epsilon: Double = 0.001) {

  private var X: List[(Double, Double)] = List(X0)

  private def iteration(): Boolean = {
    val current = X.last
    val nextX = (current._1 - stepRule.alpha(X.size, current)*df(current._1, current._2)._1,
      current._2- stepRule.alpha(X.size, current)*df(current._1, current._2)._2)
    X = X ::: List(nextX)
    continue(nextX, epsilon) && X.size < 10000
  }

  private def continue(x: (Double, Double), epsilon: Double) = sqrt(pow(df(x._1, x._2)._1, 2) + pow(df(x._1, x._2)._2, 2)) > epsilon

  def run() = {
    while (iteration()) {}
    X
  }
}

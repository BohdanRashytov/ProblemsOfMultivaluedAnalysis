package pma2

import java.lang.Math.{pow, sqrt}

import pma2.PMA2.df

sealed trait StepRule {
  def alpha(k: Int = 0, point: (Double, Double) = (0.0, 0.0)): Double
}

case class ConstantStepSize(h: Double) extends StepRule {
  override def alpha(k: Int = 0, point: (Double, Double) = (0.0, 0.0)): Double = h
}

case class ConstantStepLength(h: Double) extends StepRule {
  override def alpha(k: Int, x: (Double, Double) = (0.0, 0.0)): Double = h / sqrt(pow(df(x._1, x._2)._1, 2) + pow(df(x._1, x._2)._2, 2))
}

case class SquareSummableButNotSummable(a: Double, b: Double) extends StepRule{
  override def alpha(k: Int = 0, point: (Double, Double) = (0.0, 0.0)): Double = a / (b + k)
}

case class NonSummableDiminishing(a: Double) extends StepRule{
  override def alpha(k: Int = 0, point: (Double, Double) = (0.0, 0.0)): Double = a / sqrt(k + 1)
}
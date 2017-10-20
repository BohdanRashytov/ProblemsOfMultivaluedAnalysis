package pma2

import java.lang.Math._

object PMA2 {

  def f(x1: Double, x2: Double) = x1*x1 + x2*x2 + 5 *(abs(x1) + abs(x2)) - 7

  def df(x1: Double, x2: Double) = {
    val df1 = if (x1 > 0) 2*x1+5 else if (x1 < 0) 2*x1 - 5 else random()*10 - 5
    val df2 = if (x2 > 0) 2*x2+5 else if (x2 < 0) 2*x2 - 5 else random()*10 - 5
    (df1, df2)
  }

  def main(args: Array[String]): Unit = {
    experimentsForPoint((10.0, 9.0))
    experimentsForPoint((-5.0, 5.0))
    experimentsForPoint((-8.0, -7.0))
  }

  def experimentsForPoint(point: (Double, Double)) = {
    val solution1 = new SubGradientMethod(point, ConstantStepSize(0.1)).run()
    Graph.paint(solution1, s"SubGradientMethod-ConstantStepSize-$point")
    Writer.write(s"SubGradientMethod-ConstantStepSize-$point", solution1)

    val solution2 = new SubGradientMethod(point, ConstantStepLength(0.1)).run()
    Graph.paint(solution2, s"SubGradientMethod-ConstantStepLength-$point")
    Writer.write(s"SubGradientMethod-ConstantStepLength-$point", solution2)

    val solution3 = new SubGradientMethod(point, SquareSummableButNotSummable(1.0, 1.0)).run()
    Graph.paint(solution3, s"SubGradientMethod-SquareSummableButNotSummable-$point")
    Writer.write(s"SubGradientMethod-SquareSummableButNotSummable-$point", solution3)

    val solution4 = new SubGradientMethod(point, NonSummableDiminishing(1.0)).run()
    Graph.paint(solution4, s"SubGradientMethod-NonSummableDiminishing-$point")
    Writer.write(s"SubGradientMethod-NonSummableDiminishing-$point", solution4)

    Graph.paintAll(s"SubGradientMethod-$point", solution1, solution2, solution3, solution4)
  }

}

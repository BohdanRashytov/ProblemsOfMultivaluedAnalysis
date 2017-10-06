package pma1

import java.lang.Math.{abs, sqrt}

import PMA1._

class Newton(X0: (Double, Double), epsilon: Double = 0.0001) {

  private var X: List[(Double, Double)] = List(X0)

  private def iteration(): Boolean = {
    val X0 = X.head
    val currentX = X.last
    val det = dfx1x1(X0._1, X0._2) * dfx2x2(X0._1, X0._2) - dfx1x2(X0._1, X0._2) * dfx1x2(X0._1, X0._2)
    val deltaX1 = dfx1(currentX._1, currentX._2) * dfx2x2(X0._1, X0._2) - dfx2(currentX._1, currentX._2) * dfx1x2(X0._1, X0._2)
    val deltaX2 = -dfx1(currentX._1, currentX._2) * dfx1x2(X0._1, X0._2) + dfx2(currentX._1, currentX._2) * dfx1x1(X0._1, X0._2)
    val nextX = (currentX._1 - deltaX1 / det, currentX._2 - deltaX2 / det)
    X = X ::: List(nextX)
    continue(nextX, epsilon)
  }

  private def continue(x: (Double, Double), epsilon: Double) = sqrt(abs(dfx1(x._1, x._2)) + abs(dfx2(x._1, x._2))) > epsilon

  private def searchStartingPoint() = {
    def pos(point: (Double, Double)) = dfx1x1(point._1, point._2) > 2 && dfx1x1(point._1, point._2) * dfx2x2(point._1, point._2) - dfx1x2(point._1, point._2) * dfx1x2(point._1, point._2) > 2.5

    val n = 100
    val h = 10.0 / n
    var points = List[(Double, Double)]()
    (1 to n).foreach(ix => {
      (1 to n).foreach(iy => {
        val p1 = (ix * h, iy * h)
        val p2 = (ix * h, -iy * h)
        val p3 = (-ix * h, iy * h)
        val p4 = (-ix * h, -iy * h)

        if (pos(p1)) points = points ::: List(p1)
        if (pos(p2)) points = points ::: List(p2)
        if (pos(p3)) points = points ::: List(p3)
        if (pos(p4)) points = points ::: List(p4)
      })
    })
    Writer.writePoints("NewtonStartingPoints", points)
  }

  def run() = {
    //    searchStartingPoint()
    while (iteration()) {}
    X
  }
}

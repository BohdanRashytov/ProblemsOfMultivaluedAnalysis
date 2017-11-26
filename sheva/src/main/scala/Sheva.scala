package sheva

import java.lang.Math._

object Sheva {
  def f(x: Double, y: Double) = x*x + 3*y*y + cos(x + y)

  def dfdx(x: Double, y: Double) = 2*x  - sin(x + y)

  def dfdy(x: Double, y: Double) = 6*y  - sin(x + y)

  var Solution: List[(Double, Double)] = List((515615.156, 65416.0212))

  val epsilon = 0.0001

  val n = 100

  var globalBreak = false

  def main(args: Array[String]): Unit = {
    while (!globalBreak ) {
      val nextSol = iteration(Solution.last)
      Solution = Solution ::: List(nextSol)
    }

    println(Solution)
    println(f(Solution.last._1, Solution.last._2))
  }

  def normGrad2(x: Double, y: Double): Double = {
    val grad = (dfdx(x, y), dfdy(x, y))
    grad._1* grad._1 + grad._2* grad._2
  }

  def iteration(prevPoint: (Double, Double)): (Double, Double) = {
    var answer = (0.0, 0.0)
    var break = false
    var j = 0
    var curSolution: List[(Double, Double)] = List(prevPoint)
    var S: List[(Double, Double)] = List((
      -dfdx(curSolution.last._1, curSolution.last._2),
      -dfdy(curSolution.last._1, curSolution.last._2)))
    while (!break) {

      def functionAlpha(alpha: Double)= {
        val xA = curSolution.last
        val sA = S.last
        f(xA._1 + alpha * sA._1, xA._2 + alpha * sA._2)
      }

      val alpha = new GoldenSection(functionAlpha).solution()

      val cS = curSolution.last
      curSolution = curSolution ::: List((
        curSolution.last._1 + alpha*S.last._1,
        curSolution.last._2 + alpha*S.last._2
      ))
      val w = normGrad2(curSolution.last._1, curSolution.last._2) / normGrad2(cS._1, cS._2)
      S = S ::: List((
        -dfdx(curSolution.last._1, curSolution.last._2) + w*S.last._1,
        -dfdy(curSolution.last._1, curSolution.last._2) + w*S.last._2
      ))

      if (
        sqrt(S.last._1*S.last._1 + S.last._2+S.last._2) < epsilon ||
         sqrt( (cS._1 - curSolution.last._1) * (cS._1 - curSolution.last._1) + (cS._2 - curSolution.last._2)* (cS._2 - curSolution.last._2)) < epsilon
      ) {
        answer = (curSolution.last._1, curSolution.last._2)
        break = true
        globalBreak = true

      } else {
        if (j + 1 < n) j = j + 1
        else {
          answer = (curSolution.last._1, curSolution.last._2)
          break = true
        }
      }


    }
    answer
  }

}

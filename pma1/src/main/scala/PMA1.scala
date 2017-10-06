package pma1

import java.lang.Math._

object PMA1 {

  def f(x1: Double, x2: Double) = log(1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2))

  def dfx1(x1: Double, x2: Double) = (6 * x1 - sin(x1 - x2)) / (1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2))

  def dfx2(x1: Double, x2: Double) = (10 * x2 + sin(x1 - x2)) / (1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2))

  def dfx1x1(x1: Double, x2: Double) = (5 - 18 * x1 * x1 + 30 * x2 * x2 + (5 - 3 * x1 * x1 - 5 * x2 * x1) * cos(x1 - x2) + 12 * x1 * sin(x1 - x2)) / ((1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2)) * (1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2)))

  def dfx2x2(x1: Double, x2: Double) = (9 + 30 * x1 * x1 - 50 * x2 * x2 + (9 - 3 * x1 * x1 - 5 * x2 * x2) * cos(x1 - x2) - 20 * x2 * sin(x1 - x2)) / ((1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2)) * (1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2)))

  def dfx1x2(x1: Double, x2: Double) = (1 - 60 * x1 * x2 + (1 + 3 * x1 * x1 + 5 * x2 * x2) * cos(x1 - x2) + (-6 * x1 + 10 * x2) * sin(x1 - x2)) / ((1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2)) * (1 + 3 * x1 * x1 + 5 * x2 * x2 + cos(x1 - x2)))


  def main(args: Array[String]): Unit = {
    //    NewtonExperiments()
        GaussSeidelExperiments()
//        commonExperiments()
  }

  def NewtonExperiments() = {
    println("Newton RUN!")

    val solutionNewton1 = new Newton((0.1, 0.1)).run()
    Graph.paint(solutionNewton1, "Newton - (0.1,0.1)", true)
    //    Graph.paint(solutionNewton1, "Newton - (0.1,0.1)", false)
    Writer.write("ModifyNewtonExperiment1", solutionNewton1)

    val solutionNewton2 = new Newton((-0.1, 0.3)).run()
    Graph.paint(solutionNewton2, "Newton - (-0.1,0.3)", true)
    //    Graph.paint(solutionNewton2, "Newton - (-0.1,0.3)", false)
    Writer.write("ModifyNewtonExperiment2", solutionNewton2)

    val solutionNewton3 = new Newton((0.1, 0.2)).run()
    Graph.paint(solutionNewton3, "Newton - (0.1,0.2)", true)
    //    Graph.paint(solutionNewton3, "Newton - (0.1,0.2)", false)
    Writer.write("ModifyNewtonExperiment3", solutionNewton3)

    val solutionNewton4 = new Newton((0.2, -0.3)).run()
    Graph.paint(solutionNewton4, "Newton - (0.2,-0.3)", true)
    //    Graph.paint(solutionNewton4, "Newton - (0.2,-0.3)", false)
    Writer.write("ModifyNewtonExperiment4", solutionNewton4)

  }

  def GaussSeidelExperiments() = {
    println("GaussSeidel RUN!")

    val solutionGaussSeidel1 = new GaussSeidel((3.0, 3.0)).run()
    Graph.paint(solutionGaussSeidel1, "GaussSeidel - (2.0, 1.0)", true)
    //    Graph.paint(solutionGaussSeidel1, "GaussSeidel - (2.0, 1.0)", false)
    Writer.write("GaussSeidelExperiment1", solutionGaussSeidel1)

    val solutionGaussSeidel2 = new GaussSeidel((2.0, -2.0)).run()
    Graph.paint(solutionGaussSeidel2, "GaussSeidel - (5.0, 2.0)", true)
    //    Graph.paint(solutionGaussSeidel2, "GaussSeidel - (5.0, 2.0)", false)
    Writer.write("GaussSeidelExperiment2", solutionGaussSeidel2)

    val solutionGaussSeidel3 = new GaussSeidel((-10.0, -12.5)).run()
    Graph.paint(solutionGaussSeidel3, "GaussSeidel - (-1.0, -2.5)", true)
    //    Graph.paint(solutionGaussSeidel3, "GaussSeidel - (-1.0, -2.5)", false)
    Writer.write("GaussSeidelExperiment3", solutionGaussSeidel3)

    val solutionGaussSeidel4 = new GaussSeidel((-15.0, 22.75)).run()
    Graph.paint(solutionGaussSeidel4, "GaussSeidel - (-1.0, -2.5)", true)
    //    Graph.paint(solutionGaussSeidel4, "GaussSeidel - (-1.0, -2.5)", false)
    Writer.write("GaussSeidelExperiment4", solutionGaussSeidel4)
  }

  def commonExperiments() = {
    val solutionNewton1 = new Newton((-0.1, 0.3)).run()
    val solutionGaussSeidel1 = new GaussSeidel((-0.1, 0.3)).run()
    Graph.paint(solutionNewton1, "Common - (-0.1,0.3)", true, Some(solutionGaussSeidel1))

    val solutionNewton2 = new Newton((0.2, -0.3)).run()
    val solutionGaussSeidel2 = new GaussSeidel((0.2, -0.3)).run()
    Graph.paint(solutionNewton2, "Common - (0.2,-0.3)", true, Some(solutionGaussSeidel2))
  }
}

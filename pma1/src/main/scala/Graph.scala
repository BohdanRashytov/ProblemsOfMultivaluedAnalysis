package pma1

import java.awt.Color
import javax.swing.JFrame
import java.lang.Math._

import org.math.plot.Plot3DPanel
import PMA1._

object Graph {

  def paint(lines: List[(Double, Double)], plotName: String, paintGraph: Boolean, extraLinex: Option[List[(Double, Double)]] = None) = {
    val plot: Plot3DPanel = new Plot3DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setSize(900, 900)
    frame.setVisible(true)
    frame.setContentPane(plot)

    val xN = 200
    val yN = 200
    val m = max(abs(lines.head._1), abs(lines.head._2)) + 0.1
    val (x1, x2, y1, y2) = (-m, m, -m, m)
    val xh = (x2 - x1) / xN
    val yh = (y2 - y1) / yN

    val point = (for {
      xi <- 0 to xN
      yi <- 0 to yN
    } yield {
      val x = x1 + xi * xh
      val y = y1 + yi * yh
      List(x, y, if (paintGraph) f(x, y) else f(lines.last._1, lines.last._2) - 0.01).toArray
    }).toArray
    plot.addScatterPlot("Function", Color.RED, point)

    val solution = lines.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray
    plot.addLinePlot("Solution", Color.BLUE, solution)

    extraLinex.foreach(l => {
      val extraSolution = l.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray
      plot.addLinePlot("Solution", Color.GREEN, extraSolution)
    })
  }
}

package pma2

import java.awt.Color
import java.lang.Math._
import javax.swing.JFrame
import PMA2._
import org.math.plot.Plot3DPanel

object Graph {

  def paint(lines: List[(Double, Double)], plotName: String) = {
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
      List(x, y, f(x, y)).toArray
    }).toArray
    plot.addScatterPlot("Function", Color.RED, point)

    val solution = lines.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray
    plot.addLinePlot("Solution", Color.BLUE, solution)
  }

  def paintAll(plotName: String,
               lines1: List[(Double, Double)],
               lines2: List[(Double, Double)],
               lines3: List[(Double, Double)],
               lines4: List[(Double, Double)]) = {
    val plot: Plot3DPanel = new Plot3DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setSize(900, 900)
    frame.setVisible(true)
    frame.setContentPane(plot)

    val xN = 200
    val yN = 200
    val m = abs(lines1.head._1) max abs(lines1.head._2) max
      abs(lines2.head._1) max abs(lines2.head._2) max
      abs(lines3.head._1) max abs(lines3.head._2) max
      abs(lines4.head._1) max abs(lines4.head._2)+ 0.1
    val (x1, x2, y1, y2) = (-m, m, -m, m)
    val xh = (x2 - x1) / xN
    val yh = (y2 - y1) / yN

    val point = (for {
      xi <- 0 to xN
      yi <- 0 to yN
    } yield {
      val x = x1 + xi * xh
      val y = y1 + yi * yh
      List(x, y, f(x, y)).toArray
    }).toArray
    plot.addScatterPlot("Function", Color.RED, point)

    val solution1 = lines1.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray
    val solution2 = lines2.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray
    val solution3 = lines3.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray
    val solution4 = lines4.map(xy => List(xy._1, xy._2, f(xy._1, xy._2)).toArray).toArray

    plot.addLinePlot("Solution", Color.BLUE, solution1)
    plot.addLinePlot("Solution", Color.GREEN, solution2)
    plot.addLinePlot("Solution", Color.ORANGE, solution3)
    plot.addLinePlot("Solution", Color.MAGENTA, solution4)
  }
}

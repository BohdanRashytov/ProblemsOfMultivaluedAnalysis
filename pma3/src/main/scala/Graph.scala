package pma3

import java.awt.Color
import javax.swing.JFrame
import org.math.plot.{Plot2DPanel, Plot3DPanel}

object Graph {

  def paint3D(lines: List[(Double, Double, Double)], plotName: String) = {
    val plot: Plot3DPanel = new Plot3DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setSize(900, 900)
    frame.setVisible(true)
    frame.setContentPane(plot)

    plot.addScatterPlot("Solution", Color.BLUE, lines.map(e => Array(e._1, e._2, e._3)).toArray)
  }

  def paint2D(lines: List[(Double, Double)], plotName: String) = {
    val plot: Plot2DPanel = new Plot2DPanel()
    val frame: JFrame = new JFrame(plotName)
    frame.setSize(900, 900)
    frame.setVisible(true)
    frame.setContentPane(plot)

    plot.addScatterPlot("Solution", Color.BLUE, lines.map(e => Array(e._1, e._2)).toArray)
  }
}

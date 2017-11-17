package pma3

import Jama.Matrix

class Experiment(private val expName: String = "Experiment",
                 private val M1: Double = 1.0,
                 private val M2: Double = 1.0,

                 private val K: Double = 1000.0,
                 private val K1: Double = 1500.0,
                 private val K2: Double = 1500.0,
                 private val B: Double = 150.0,
                 private val B1: Double = 100.0,
                 private val B2: Double = 100.0,

                 private val to: Int = 10,
                 private val T: Int = 1000,
                 private val h: Double = 0.0001,

                 private val f1: Double => Double = (t: Double) => t / 1000.0,
                 private val f2: Double => Double = (t: Double) => t / 1000.0,

                 private val X0: Matrix = new Matrix(Array(0.25, 0.25, 1.0, 1.0), 4),
                 private val Q0: Matrix = new Matrix(Array(
                   Array(2.0, -1.0, 0.0, 1.0),
                   Array(-1.0, 2.0, -1.0, -1.0),
                   Array(0.0, -1.0, 2.0, 2.0),
                   Array(1.0, -1.0, 2.0, 10.0)
                 )),
                 private val d: Int = 50) {

  private def f(t: Double) = new Matrix(Array(0.0, 0.0, f1(t), f2(t)), 4)

  private val A = new Matrix(
    Array(
      Array(0.0, 0.0, 1.0, 0.0),
      Array(0.0, 0.0, 0.0, 1.0),
      Array(-(K + K1), K, -(B + B1), B),
      Array(K, -(K + K1), B, -(B + B2))
    )
  )

  private var Xi: List[Matrix] = List(X0)
  private var Qi: List[Matrix] = List(Q0)

  private def dxdt(x: Matrix, t: Double) = A.times(x).plus(f(t))

  private def dQdt(Q: Matrix) = A.times(Q).plus(Q.times(A.transpose()))

  private def runExperiment() = {
    (to to T).foreach(i => {
      val xPrev = Xi.last
      val QPrev = Qi.last

      val xNext = xPrev.plus(dxdt(xPrev, i).times(h))
      val QNext = QPrev.plus(dQdt(QPrev).times(h))

      Xi = Xi ::: List(xNext)
      Qi = Qi ::: List(QNext)
    })
  }

  private def mul(x: Array[Double], y: Array[Double]) = (0 until x.length).map(i => x(i) * y(i)).sum

  def run(): Unit = {

    runExperiment()

    val inverseQ = Qi.last.inverse()
    val x = Xi.last
    val condition: Matrix => Boolean = xx => mul(inverseQ.times(xx.minus(x)).getColumnPackedCopy, xx.minus(x).getColumnPackedCopy) <= 1

    var sol = List[(Double, Double, Double, Double)]()

    sol = (for {
      i1 <- -d to d // by 5
      i2 <- -d to d // by 5
      i3 <- -d to d // by 5
      i4 <- -d to d // by 5
    } yield {
      val testX = new Matrix(Array(1.0 * i1 / 1.0, 1.0 * i2 / 1.0, 1.0 * i3 / 1.0, 1.0 * i4 / 1.0), 4)
      if (condition(testX)) {
        List(testX)
      } else List()
    })
      .toList.flatten.map(_.getColumnPackedCopy).map(a => (a(0), a(1), a(2), a(3)))


    Writer.write(expName, {
      val xx = x.getColumnPackedCopy.toList
      (xx(0), xx(1), xx(2), xx(3))
    }, Qi.last.getArray.toList.map(_.toList), sol)

    Graph.paint3D(sol.map(e => (e._1, e._3, e._4)), s"$expName - graph3D")
    Graph.paint2D(sol.map(e => (e._3, e._4)), s"$expName - graph2D")
  }
}

package sheva

import java.lang.Math._

class GoldenSection(f: Double => Double, a0: Double = 1000.0, b0: Double = 1000.0) {

  private val tau = (sqrt(5) - 1.0) / 2.0
  private val epsilon = 0.001

  def solution() = {
    var a = a0
    var b = b0
    var lambda = a + (1 - tau) * (b - a)
    var mu = a + tau * (b - a)

    while (abs(b - a) > epsilon) {
      if (f(lambda) > f(mu)) {
        a = lambda
        lambda = mu
        mu = a + tau * (b - a)
      } else {
        b = mu
        mu = lambda
        lambda = a + (1 - tau) * (b - a)
      }
    }
    (a + b) / 2
  }
}

package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double]
  ): Signal[Double] = Signal(scala.math.pow(b(), 2) - 4 * a() * c())

  def computeSolutions(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double],
      delta: Signal[Double]
  ): Signal[Set[Double]] = Signal {
    val deltaVal = delta()
    if (deltaVal < 0){
      Set()
    }
    else {
    val negB =  - b()
    val sqDelta = scala.math.sqrt(deltaVal)
    val denom = 2 * a()
    Set(
      (negB + sqDelta) / denom,
      (negB - sqDelta) / denom
    )
    }
  }
}

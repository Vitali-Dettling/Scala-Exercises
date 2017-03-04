package calculator

object Polynomial {

  def sr(x: Double): Double = Math.sqrt(x)

  //Δ = b² - 4ac
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal(Math.pow(b(), 2) - 4*a()*c())

  //(-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal( Set( (-b() + sr(delta()))/2*a() ,  (-b() - sr(delta()))/2*a()   ) )
  }
}

package nature.vector


case class PVector(x: Float, y: Float) {

  def +(v: PVector) = PVector(x + v.x, y + v.y)

  def -(v: PVector) = PVector(x - v.x, y - v.y)

  def *(n: Float) = PVector(x * n, y * n)

  def /(n: Float) = PVector(x / n, y / n)

  def mag = Math.sqrt(x * x + y * y).toFloat

  def normalize = if (mag > 0) this / mag else this

  def map[A](f: PVector => A): A = f(this)

  override def toString = s"vx = $x ; vy = $y"
}

object  PVector {

  def randomNormCoordinate = ((Math.random() * 2) - 1).toFloat

  def randomNorm2D: PVector = PVector(randomNormCoordinate, randomNormCoordinate)

}

object ZeroVector extends PVector(0, 0)

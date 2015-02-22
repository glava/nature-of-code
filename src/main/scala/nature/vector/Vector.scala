import processing.core._

object BouncingBall extends App {

   PRunner.run(new BouncingBall, "Bouncing Ball")
}

class BouncingBall extends PApplet {

  var ballLocation = PVector(100, 100)
  var velocity = PVector(2, 5)

  override def setup() = {
    size(640, 360)
    background(255)
  }

  override def draw() = {
    background(255)

    ballLocation = ballLocation + velocity

    println(ballLocation)

    if((ballLocation.x > width) || (ballLocation.x < 0)) {
      velocity = velocity.copy(x = (-1) * velocity.x)
    }

    if((ballLocation.y > height) || (ballLocation.y < 0)) {
      velocity  = velocity.copy(y = (-1) * velocity.y)
    }

    stroke(0)
    fill(175)
    ellipse(ballLocation.x, ballLocation.y, 16, 16)

  }
}

case class PVector(x: Float, y: Float) {
  def +(v: PVector) = PVector(x + v.x, y + v.y)

  override def toString = s"vx = $x ; vy = $y"
}


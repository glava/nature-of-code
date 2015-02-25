import processing.core._

case class PVector(x: Float, y: Float, limit: Option[Float] = None) {

  def +(v: PVector) = withLimit(PVector(x + v.x, y + v.y, limit))

  def -(v: PVector) = withLimit(PVector(x - v.x, y - v.y, limit))

  def *(n: Float) = withLimit(PVector(x * n, y * n, limit))

  def /(n: Float) = withLimit(PVector(x / n, y / n, limit))

  def mag = Math.sqrt(x * x + y * y).toFloat

  def normalize  = if (mag > 0) this / mag else this

  def withLimit(v: PVector) = limit.fold(v)(l => if (l > v.mag) v else this)

  override def toString = s"vx = $x ; vy = $y"
}

object PVector {

  def randomNormCoordinate = ((Math.random() * 2) - 1).toFloat

  def randomNorm2D: PVector = PVector(randomNormCoordinate, randomNormCoordinate)

}

object ZeroVector extends PVector(0, 0)


class VectorSubtraction extends PApplet {

  override def draw() = {
    background(255)

    val lineVector = (PVector(mouseX, mouseY) - PVector(width / 2, height / 2)) * 0.5f

    fill(0)
    rect(0f, 0f, PVector(mouseX, mouseY).mag, 10f )

    translate(width / 2, height / 2)
    line(0, 0, lineVector.x, lineVector.y)
  }

}

object VectorSubtraction extends App {
  PRunner.run(new VectorSubtraction, "Subtraction")
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

    if ((ballLocation.x > width) || (ballLocation.x < 0)) {
      velocity = velocity.copy(x = (-1) * velocity.x)
    }

    if ((ballLocation.y > height) || (ballLocation.y < 0)) {
      velocity = velocity.copy(y = (-1) * velocity.y)
    }

    stroke(0)
    fill(175)
    ellipse(ballLocation.x, ballLocation.y, 16, 16)

  }
}

case class Ball(location: PVector, velocity: PVector, applet: PApplet) {

  def move = Ball(location + velocity, velocity + PVector.randomNorm2D, applet)

  def display() = {
    applet.stroke(0)
    applet.fill(175)
    applet.ellipse(location.x, location.y, 16, 16)
  }

  def keepInBounds =  {
    val r =
    if (location.x > applet.width) {
      copy(location = location.copy(x = 0))
    } else if (location.x < 0) {
      copy(location = location.copy(x = applet.width))
    } else this

    if (r.location.y > applet.height) {
      copy(location = location.copy(y = 0))
    } else if (location.y < 0) {
      copy(location = location.copy(y = applet.height))
    } else r
  }
}

class BallThroughWalls extends PApplet {

  var mover = Ball(
    location =  PVector(random(width),random(height)) ,
    velocity = PVector(random(-2,2),random(-2,2), Some(10)),
    applet  = this
  )

  override def setup() = {
    size(640, 360)
    background(255)
  }

  override def draw() = {
    background(255)
    mover = mover.move.keepInBounds
    mover.display()
  }
}

object BallThroughWalls extends App {
  PRunner.run(new BallThroughWalls, "Ball goes through walls")
}

object BouncingBall extends App {
  PRunner.run(new BouncingBall, "Bouncing Ball")
}





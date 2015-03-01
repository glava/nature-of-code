import processing.core._
import nature.vector.{PVector, ZeroVector}

class VectorSubtraction extends PApplet {

  override def draw() = {
    background(255)

    val lineVector = (PVector(mouseX, mouseY) - PVector(width / 2, height / 2)) * 0.5f

    fill(0)
    rect(0f, 0f, PVector(mouseX, mouseY).mag, 10f)

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

  def move(acc: PVector) = {
    (velocity + acc).map{ speed =>
      if (speed.mag >= 5)
        Ball(location + velocity, velocity, applet)
      else
        Ball(location + speed, speed, applet)
    }
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

  override def toString = velocity.mag.toString
}

class BallThroughWalls extends PApplet {

  var mover = Ball(
    location = PVector(random(width), random(height)),
    velocity = PVector(0, 0),
    applet = this
  )

  override def setup() = {
    size(640, 360)
    background(255)
  }

  override def draw() = {
    background(255)
    mover = mover.move(PVector.randomNorm2D).keepInBounds
  }
}

class BallDrawnToMouse extends PApplet {

  var mover = Ball(
    location = PVector(random(width), random(height)),
    velocity = ZeroVector,
    applet = this
  )

  override def setup() = {
    size(640, 360)
    background(255)
  }

  override def draw() = {
    background(255)

    val norm = (PVector(mouseX.toFloat, mouseY.toFloat) - mover.location).normalize * 0.5f
    mover = mover.move(norm).keepInBounds

    stroke(0)
    fill(175)
    ellipse(mover.location.x, mover.location.y, 16, 16)
  }
}

class ListBallDrawnToMouse extends PApplet {

  var mover: List[Ball] = List.fill(10) {
    Ball(
      location = PVector(random(640), random(360)),
      velocity = ZeroVector,
      applet = this
    )
  }

  println(mover)

  override def setup() = {
    size(640, 360)
    background(255)
  }

  override def draw() = {
    background(255)

    def norm(ball: Ball) = (PVector(mouseX.toFloat, mouseY.toFloat) - ball.location).normalize * 0.5f

    mover = mover.map(b => b.move(norm(b)).keepInBounds)

    mover.foreach { b =>
      stroke(0)
      fill(175)
      ellipse(b.location.x, b.location.y, 16, 16)
    }
  }
}

object BallThroughWalls extends App {
  PRunner.run(new BallThroughWalls, "Ball goes through walls")
}

object BouncingBall extends App {
  PRunner.run(new BouncingBall, "Bouncing Ball")
}

object BallDrawnToMouse extends App {
  PRunner.run(new BallDrawnToMouse, "Ball Drawn To Mouse")
}

object ListBallDrawnToMouse extends App {
  PRunner.run(new ListBallDrawnToMouse, "List Ball Drawn To Mouse")
}





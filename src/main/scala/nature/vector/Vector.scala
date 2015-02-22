import processing.core._

object BouncingBall extends App {

   PRunner.run(new BouncingBall, "Bouncing Ball")
}

class BouncingBall extends PApplet {

  var ballX = 100
  var ballY = 100
  var xspeed = 1
  var yspeed = 3

  override def setup() = {
    size(640, 360)
    background(255)
  }

  override def draw() = {
    background(255)

    ballX = ballX + xspeed
    ballY = ballY + yspeed

    if((ballX > width) || (ballX < 0)) {
      xspeed  = xspeed * (-1)
    }

    if((ballY > height) || (ballY < 0)) {
      yspeed  = yspeed * (-1)
    }

    stroke(0)
    fill(175)
    ellipse(ballX, ballY, 16, 16)

  }
}


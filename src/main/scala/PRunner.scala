import processing.core._

object PRunner extends PApplet {

  def run(app: PApplet, title: String) = {
    val frame = new javax.swing.JFrame(title)
    frame.getContentPane.add(app)
    app.init()
    frame.pack()
    frame.setVisible(true)
  }
}

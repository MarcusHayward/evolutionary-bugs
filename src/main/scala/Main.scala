import scala.swing._

object Main extends App {
  val dimension = 10
  val map = Map.generateRandom(dimension)

  def info(map: Map) = {
    map.pieces.map(
      x => println(s"(${x.coordinates._1}, ${x.coordinates._2}) is a ${x.pieceType}")
    )
  }

  def render(map: Map) = {
    map.pieces.map(
      piece => piece.coordinates._2 match {
        case x if (x + 1) == map.dimension => println(piece.pieceType)
        case _ => print(piece.pieceType)
      }
    )
  }

  val ui = new UI
  ui.visible = true

  render(map)
}

class UI extends MainFrame {
  title = "GUI Program #1"
  preferredSize = new Dimension(320, 240)
  contents = new Label("Here is the contents!")
}
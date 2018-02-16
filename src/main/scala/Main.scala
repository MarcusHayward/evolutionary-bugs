import javax.swing.ImageIcon

import scala.swing._

object Main extends App {
  val dimension = 40
  val world = World.generateRandom(dimension)

  def info(world: World) = {
    world.pieces.map(
      x => println(s"(${x.coordinates._1}, ${x.coordinates._2}) is a ${x.pieceType}")
    )
  }

  def render(world: World) = {
    world.pieces.map(
      piece => piece.coordinates._2 match {
        case x if (x + 1) == world.dimension => println(piece.pieceType)
        case _ => print(piece.pieceType)
      }
    )
  }

  val ui = new UI
  ui.renderWorld(world)
  ui.visible = true
}

class UI extends MainFrame {
  title = "GUI Program #1"
  preferredSize = new Dimension(20 * Main.dimension, 20 * Main.dimension)
  def renderWorld(world: World) = {
    val components = world.pieces.map {
      piece => {
        new Label {
          icon = new ImageIcon(piece.pieceType.imagePath)
        }
      }
    }

    contents = new GridPanel(Main.dimension, Main.dimension) {
      contents.appendAll(components)
    }
  }
}


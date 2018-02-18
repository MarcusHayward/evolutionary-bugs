import javax.swing.ImageIcon

import scala.swing._
import scala.swing.event._

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

  def drawWorld(world: World): Unit = {
    val ui = new UI
    ui.renderWorld(world)
    ui.visible = true
  }

  val player = Piece(Player, (0, 0))
  val worldWithPlayer = world.withPlayer(player)
  drawWorld(worldWithPlayer)
}

class UI extends MainFrame {
  title = "Evolutionary Bugs"
  preferredSize = new Dimension(20 * Main.dimension, 20 * Main.dimension)

  def renderWorld(world: World): Unit = {
    val components = world.pieces.map {
      piece => {
        new Label {
          icon = new ImageIcon(piece.pieceType.imagePath)
        }
      }
    }

    contents = new GridPanel(Main.dimension, Main.dimension) {
      focusable = true
      listenTo(keys)
      reactions += {
        case KeyTyped(_, c, _, _) => c match {
          case 'w' =>
            val newWorld = world.movePlayer(world, Up)
            Main.drawWorld(newWorld)
          case 's' =>
            val newWorld = world.movePlayer(world, Down)
            Main.drawWorld(newWorld)
          case 'a' =>
            val newWorld = world.movePlayer(world, Left)
            Main.drawWorld(newWorld)
          case 'd' =>
            val newWorld = world.movePlayer(world, Right)
            Main.drawWorld(newWorld)
        }
      }
      contents.appendAll(components)
    }
  }
}
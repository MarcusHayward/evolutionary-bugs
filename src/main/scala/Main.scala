import javax.swing.ImageIcon

import scala.swing._
import scala.swing.event._

object Main extends App {
  val dimension = 40
  val world = World.generateRandom(dimension)

  def drawWorld(world: World): Unit = {
    val ui = new UI
    ui.renderWorld(world)
    ui.visible = true
  }

  val player = Piece(Player, (0, 0))
  val worldWithPlayer = world.withPlayer(player)
  drawWorld(worldWithPlayer)

  while (true) {
    val ai = Ai("fake")
    val move = ai.generateMove(worldWithPlayer)
    val newWorld = worldWithPlayer.movePlayer(move)
    drawWorld(newWorld)
    Thread.sleep(1000)
  }
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
        case KeyTyped(_, 'w', _, _) =>
          val newWorld = world.movePlayer(Up)
          Main.drawWorld(newWorld)
        case KeyTyped(_, 's', _, _) =>
          val newWorld = world.movePlayer(Down)
          Main.drawWorld(newWorld)
        case KeyTyped(_, 'a', _, _) =>
          val newWorld = world.movePlayer(Left)
          Main.drawWorld(newWorld)
        case KeyTyped(_, 'd', _, _) =>
          val newWorld = world.movePlayer(Right)
          Main.drawWorld(newWorld)
      }

      contents.appendAll(components)
    }
  }
}
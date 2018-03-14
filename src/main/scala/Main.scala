import javax.swing.ImageIcon

import scala.swing._
import scala.swing.event.KeyTyped

object Main extends App {
  val dimension = 40
  val startingHealth = 100
  val startingThirst = 100
  val world = World.generateRandom(dimension)
  val ui = new UI

  def drawWorld(world: World, player: Piece): Unit = {
    ui.renderWorld(world, player)
    ui.visible = true
  }

  val player = Piece(Player(100, 100), (0, 0))
  val worldWithPlayer = world.withPlayer(player)
  drawWorld(worldWithPlayer, player)

  def run(world: World, player: Piece): Unit = {
    val ai = Ai("fake")
    val move = ai.generateMove(world)
    val newWorld: (World, Piece) = world.movePlayer(move, player)
    ui.renderWorld(newWorld._1, newWorld._2)
    Thread.sleep(10)

    run(newWorld._1, newWorld._2)
  }

  run(worldWithPlayer, player)
}

class UI extends MainFrame {
  title = "Evolutionary Bugs"
  preferredSize = new Dimension(20 * Main.dimension, 20 * Main.dimension)

  def renderWorld(world: World, player: Piece): Unit = {
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
          val newWorld = world.movePlayer(Up, player)
          Main.drawWorld(newWorld._1, newWorld._2)
        case KeyTyped(_, 's', _, _) =>
          val newWorld = world.movePlayer(Down, player)
          Main.drawWorld(newWorld._1, newWorld._2)
        case KeyTyped(_, 'a', _, _) =>
          val newWorld = world.movePlayer(Left, player)
          Main.drawWorld(newWorld._1, newWorld._2)
        case KeyTyped(_, 'd', _, _) =>
          val newWorld = world.movePlayer(Right, player)
          Main.drawWorld(newWorld._1, newWorld._2)
      }

      contents.appendAll(components)
    }
  }
}
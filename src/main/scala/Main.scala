import javax.swing.ImageIcon

import scala.swing._

object Main extends App {
  val dimension = 40
  val startingHealth = 100
  val startingThirst = 100
  val world = World.generateRandom(dimension)
  val ui = new UI

  def drawWorld(world: World): Unit = {
    ui.renderWorld(world)
    ui.visible = true
  }

  val player = Piece(Player(100, 100), (0, 0))
  val worldWithPlayer = world.withPlayer(player)
  drawWorld(worldWithPlayer)

  def run(world: World, player: Piece): Unit = {
    val ai = Ai("fake")
    val move = ai.generateMove(worldWithPlayer)
    val newWorld: (World, Piece) = worldWithPlayer.movePlayer(move, player)
    ui.renderWorld(newWorld._1)
    Thread.sleep(10)

    run(newWorld._1, newWorld._2)
  }

  run(worldWithPlayer, player)
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
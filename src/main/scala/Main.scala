package bugs

import javax.swing.ImageIcon

import scala.swing._

object Main extends App {
  val dimension = 40
  val startingHealth = 100
  val startingThirst = 100
  val world = World.generateRandom(dimension)
  val ui = new UI

  def drawWorld(world: World, player: Piece, score: Int): Unit = {
    ui.renderWorld(world, player, score)
    ui.visible = true
  }

  val player = Piece(Player(startingHealth, startingThirst), (0, 0))
  val worldWithPlayer = world.withPlayer(player)
  drawWorld(worldWithPlayer, player, 0)

  def run(world: World, player: Piece, score: Int): Unit = {
    val ai = Ai("fake")
    val move = ai.generateMove(world)
    val newWorld: (World, Piece) = world.movePlayer(move, player)
    ui.renderWorld(newWorld._1, newWorld._2, score)
    Thread.sleep(10)

    run(newWorld._1, newWorld._2, score + 1)
  }

  run(worldWithPlayer, player, 0)
}

class UI extends MainFrame {
  title = "Evolutionary Bugs"
  preferredSize = new Dimension(20 * Main.dimension, 23 * Main.dimension)

  def renderWorld(world: World, player: Piece, score: Int): Unit = {
    val components = world.pieces.map {
      piece => {
        new Label {
          icon = new ImageIcon(piece.pieceType.imagePath)
        }
      }
    }
    val box = new BoxPanel(Orientation.Vertical)
    box.contents += new Label("Health: 100")
    box.contents += new Label("Thirst: 100")
    box.contents += new Label(s"Score: $score")

    val gridPanel = new GridPanel(Main.dimension, Main.dimension)
    gridPanel.contents.appendAll(components)
    gridPanel.border = Swing.EmptyBorder(10, 10, 10, 10)

    box.contents += gridPanel
    box.border = Swing.EmptyBorder(10, 10, 10, 10)

    contents = box
  }
}
